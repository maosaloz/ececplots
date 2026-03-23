Attribute VB_Name = "ececplots_Macro"
'
' ececplots Excel Macro
' =====================
' Companion macro for the ececplots R package.
'
' PURPOSE
' -------
' Imports PNG/PDF images previously exported by the R package (via ecec_save)
' and optionally attaches the underlying data as a separate worksheet.
'
' USAGE
' -----
' 1. Open Excel and press Alt + F11 to open the VBA editor.
' 2. In the editor, choose Insert > Module and paste this entire file.
' 3. Close the editor and run the macro via Alt + F8 > ImportEcecPlots > Run.
'
' FUNCTIONS PROVIDED
' ------------------
'   ImportEcecPlots          – main entry point; prompts for an image file and
'                              optional data CSV, then creates a formatted
'                              Excel workbook.
'   InsertPlotToSheet        – inserts a single image file into a worksheet.
'   InsertDataToSheet        – imports a CSV file as a new data worksheet.
'   FormatDataSheet          – auto-fits columns and applies a table style.
'   CreateSummarySheet       – adds a cover/summary sheet listing all plots.
'   ExportAllPlotsFromFolder – bulk-import every PNG in a chosen folder.
'

Option Explicit

' ---------------------------------------------------------------------------
' Constants
' ---------------------------------------------------------------------------
Private Const ECEC_BLUE  As String = "4472C4"   ' #4472C4
Private Const ECEC_GREY  As String = "F2F2F2"   ' #F2F2F2
Private Const HEADER_ROW As Integer = 1

' ===========================================================================
' PUBLIC ENTRY POINTS
' ===========================================================================

' ---------------------------------------------------------------------------
' ImportEcecPlots
' Main dialog-driven workflow.  Prompts the user to:
'   (a) choose one PNG/PDF image exported by the R package
'   (b) optionally choose a CSV file with the underlying data
' Then creates (or reuses) a workbook with:
'   - A "Plot_<name>" sheet containing the image
'   - A "Data_<name>" sheet with the CSV data  (if provided)
'   - A "Summary" sheet listing all imported plots
' ---------------------------------------------------------------------------
Public Sub ImportEcecPlots()

    Dim wb          As Workbook
    Dim imagePath   As String
    Dim dataPath    As String
    Dim plotName    As String

    ' Select image file
    imagePath = GetFilePath( _
        Title      := "Select an ececplots image (PNG or PDF)", _
        FileFilter := "Image Files (*.png;*.jpg;*.jpeg;*.emf;*.wmf)," & _
                      "*.png;*.jpg;*.jpeg;*.emf;*.wmf," & _
                      "All Files (*.*),*.*")
    If imagePath = "" Then
        MsgBox "No image selected.  Operation cancelled.", vbInformation, "ececplots"
        Exit Sub
    End If

    ' Derive a clean name from the file
    plotName = CleanSheetName( _
                   Left(FileBaseName(imagePath), 20))

    ' Optional: select data CSV
    dataPath = GetFilePath( _
        Title      := "Select the data CSV for this plot (optional – Cancel to skip)", _
        FileFilter := "CSV Files (*.csv),*.csv,All Files (*.*),*.*")

    ' Work with the active workbook, or create a new one
    If ActiveWorkbook Is Nothing Then
        Set wb = Workbooks.Add
    Else
        Set wb = ActiveWorkbook
    End If

    ' Insert the plot
    InsertPlotToSheet wb, imagePath, "Plot_" & plotName

    ' Insert data if a CSV was chosen
    If dataPath <> "" Then
        InsertDataToSheet wb, dataPath, "Data_" & plotName
    End If

    ' Refresh the summary sheet
    CreateSummarySheet wb

    MsgBox "Done!  Plot """ & plotName & """ imported successfully.", _
           vbInformation, "ececplots"

End Sub

' ---------------------------------------------------------------------------
' ExportAllPlotsFromFolder
' Batch-import every PNG image in a user-selected folder.
' ---------------------------------------------------------------------------
Public Sub ExportAllPlotsFromFolder()

    Dim wb        As Workbook
    Dim folderPath As String
    Dim fileName  As String
    Dim plotName  As String
    Dim count     As Integer

    ' Let the user pick a folder
    With Application.FileDialog(msoFileDialogFolderPicker)
        .Title = "Select folder containing ececplots PNG images"
        If .Show <> -1 Then
            MsgBox "No folder selected.  Operation cancelled.", _
                   vbInformation, "ececplots"
            Exit Sub
        End If
        folderPath = .SelectedItems(1)
    End With

    If ActiveWorkbook Is Nothing Then
        Set wb = Workbooks.Add
    Else
        Set wb = ActiveWorkbook
    End If

    count    = 0
    fileName = Dir(folderPath & "\*.png")
    Do While fileName <> ""
        plotName = CleanSheetName(Left(FileBaseName(fileName), 20))
        InsertPlotToSheet wb, folderPath & "\" & fileName, "Plot_" & plotName
        count    = count + 1
        fileName = Dir
    Loop

    If count = 0 Then
        MsgBox "No PNG files found in the selected folder.", _
               vbExclamation, "ececplots"
    Else
        CreateSummarySheet wb
        MsgBox count & " plot(s) imported successfully.", _
               vbInformation, "ececplots"
    End If

End Sub

' ===========================================================================
' INTERNAL HELPERS
' ===========================================================================

' ---------------------------------------------------------------------------
' InsertPlotToSheet
' Creates (or clears) a worksheet called sheetName, inserts the image file
' centred in a nice frame, and adds a title row.
' ---------------------------------------------------------------------------
Public Sub InsertPlotToSheet(wb As Workbook, _
                              imagePath As String, _
                              sheetName As String)

    Dim ws   As Worksheet
    Dim pic  As Picture
    Dim rng  As Range

    ' Get or create the target sheet
    Set ws = GetOrCreateSheet(wb, sheetName)
    ws.Cells.Clear

    ' ── Title row ──────────────────────────────────────────────────────────
    With ws.Cells(1, 1)
        .Value         = FileBaseName(imagePath)
        .Font.Bold     = True
        .Font.Size     = 14
        .Font.Color    = RGB(68, 114, 196)   ' ECEC blue
        .Interior.Color = RGB(242, 242, 242) ' light grey background
    End With
    ws.Range("A1:J1").Merge
    ws.Rows(1).RowHeight = 28

    ' ── Separator row ──────────────────────────────────────────────────────
    ws.Rows(2).RowHeight = 6

    ' ── Insert the image ───────────────────────────────────────────────────
    Set rng = ws.Range("A3")
    Set pic = ws.Pictures.Insert(imagePath)
    With pic
        .Top    = rng.Top + 4
        .Left   = rng.Left + 4
        ' Scale proportionally to fit within ~700 x 500 pt
        Dim maxW As Double: maxW = 700
        Dim maxH As Double: maxH = 500
        Dim scaleW As Double: scaleW = maxW / .Width
        Dim scaleH As Double: scaleH = maxH / .Height
        Dim scale  As Double: scale  = Application.Min(scaleW, scaleH, 1)
        .Width  = .Width  * scale
        .Height = .Height * scale
        .Placement = xlMoveAndSize
    End With

    ' ── Footer ─────────────────────────────────────────────────────────────
    Dim footRow As Long
    footRow = ws.Range("A3").Row + _
              Int(pic.Height / ws.StandardHeight) + 2
    ws.Cells(footRow, 1).Value = _
        "Generated by ececplots R package – " & Format(Now, "yyyy-mm-dd")
    ws.Cells(footRow, 1).Font.Color = RGB(150, 150, 150)
    ws.Cells(footRow, 1).Font.Italic = True

    ws.Activate
    ws.Range("A1").Select

End Sub

' ---------------------------------------------------------------------------
' InsertDataToSheet
' Imports a CSV file into a new (or existing) worksheet and formats it as
' an Excel table.
' ---------------------------------------------------------------------------
Public Sub InsertDataToSheet(wb As Workbook, _
                              csvPath As String, _
                              sheetName As String)

    Dim ws       As Worksheet
    Dim qt       As QueryTable
    Dim listObj  As ListObject

    Set ws = GetOrCreateSheet(wb, sheetName)
    ws.Cells.Clear

    ' Use a QueryTable to import the CSV
    Set qt = ws.QueryTables.Add( _
        Connection := "TEXT;" & csvPath, _
        Destination := ws.Range("A1"))

    With qt
        .TextFileParseType      = xlDelimited
        .TextFileCommaDelimiter = True
        .TextFileColumnDataTypes = Array(1)  ' general format for all cols
        .AdjustColumnWidth      = True
        .Refresh BackgroundQuery:=False
        .Delete   ' remove the QueryTable but keep the data
    End With

    ' Format as a table if data is present
    If ws.Cells(1, 1).Value <> "" Then
        Dim lastRow  As Long: lastRow  = ws.Cells(ws.Rows.Count, 1).End(xlUp).Row
        Dim lastCol  As Long: lastCol  = ws.Cells(1, ws.Columns.Count).End(xlToLeft).Column
        Dim dataRange As Range
        Set dataRange = ws.Range(ws.Cells(1, 1), ws.Cells(lastRow, lastCol))

        Set listObj = ws.ListObjects.Add( _
            SourceType   := xlSrcRange, _
            Source       := dataRange, _
            XlListObjectHasHeaders := xlYes)
        listObj.TableStyle = "TableStyleMedium2"
        FormatDataSheet ws
    End If

End Sub

' ---------------------------------------------------------------------------
' FormatDataSheet
' Auto-fits all columns and freezes the header row.
' ---------------------------------------------------------------------------
Private Sub FormatDataSheet(ws As Worksheet)
    ws.Cells.EntireColumn.AutoFit
    With ws
        .Range("A2").Select
        .Application.ActiveWindow.FreezePanes = True
    End With
    ws.Range("A1").Select
End Sub

' ---------------------------------------------------------------------------
' CreateSummarySheet
' Creates or updates a "Summary" sheet that lists all Plot_ sheets with
' hyperlinks, the date imported, and the source file name.
' ---------------------------------------------------------------------------
Public Sub CreateSummarySheet(wb As Workbook)

    Dim ws     As Worksheet
    Dim wsPlot As Worksheet
    Dim row    As Long

    Set ws = GetOrCreateSheet(wb, "Summary")
    ws.Cells.Clear

    ' ── Header ─────────────────────────────────────────────────────────────
    With ws.Rows(1)
        .RowHeight = 30
        .Interior.Color = RGB(68, 114, 196)
    End With
    With ws.Cells(1, 1)
        .Value      = "ececplots – Figure Summary"
        .Font.Bold  = True
        .Font.Size  = 16
        .Font.Color = RGB(255, 255, 255)
    End With
    ws.Range("A1:D1").Merge

    ' ── Column headers ─────────────────────────────────────────────────────
    row = 2
    ws.Cells(row, 1).Value = "Plot Sheet"
    ws.Cells(row, 2).Value = "Data Sheet"
    ws.Cells(row, 3).Value = "Date Imported"
    ws.Cells(row, 4).Value = "Notes"
    With ws.Rows(row)
        .Font.Bold = True
        .Interior.Color = RGB(197, 217, 241)
    End With

    ' ── Data rows ──────────────────────────────────────────────────────────
    row = 3
    For Each wsPlot In wb.Sheets
        If Left(wsPlot.Name, 5) = "Plot_" Then
            Dim plotBaseName As String
            plotBaseName = Mid(wsPlot.Name, 6)

            ' Hyperlink to plot sheet
            ws.Hyperlinks.Add _
                Anchor   := ws.Cells(row, 1), _
                Address  := "", _
                SubAddress := "'" & wsPlot.Name & "'!A1", _
                TextToDisplay := wsPlot.Name

            ' Link to data sheet if it exists
            On Error Resume Next
            Dim wsData As Worksheet
            Set wsData = wb.Sheets("Data_" & plotBaseName)
            On Error GoTo 0
            If Not wsData Is Nothing Then
                ws.Hyperlinks.Add _
                    Anchor   := ws.Cells(row, 2), _
                    Address  := "", _
                    SubAddress := "'Data_" & plotBaseName & "'!A1", _
                    TextToDisplay := "Data_" & plotBaseName
                Set wsData = Nothing
            Else
                ws.Cells(row, 2).Value = "—"
            End If

            ws.Cells(row, 3).Value  = Format(Now, "yyyy-mm-dd hh:mm")
            ws.Cells(row, 3).NumberFormat = "yyyy-mm-dd hh:mm"

            row = row + 1
        End If
    Next wsPlot

    ' ── Formatting ─────────────────────────────────────────────────────────
    ws.Columns("A:D").AutoFit
    ws.Range("A1").Select
    ws.Activate

End Sub

' ===========================================================================
' UTILITY FUNCTIONS
' ===========================================================================

' ---------------------------------------------------------------------------
' GetOrCreateSheet – returns an existing sheet or creates a new one.
' ---------------------------------------------------------------------------
Private Function GetOrCreateSheet(wb As Workbook, _
                                   name As String) As Worksheet
    Dim ws As Worksheet
    On Error Resume Next
    Set ws = wb.Sheets(name)
    On Error GoTo 0
    If ws Is Nothing Then
        Set ws = wb.Sheets.Add(After:=wb.Sheets(wb.Sheets.Count))
        ws.Name = name
    End If
    Set GetOrCreateSheet = ws
End Function

' ---------------------------------------------------------------------------
' GetFilePath – wraps Application.GetOpenFilename for cleaner calling.
' ---------------------------------------------------------------------------
Private Function GetFilePath(Title As String, _
                              FileFilter As String) As String
    Dim result As Variant
    result = Application.GetOpenFilename( _
                 FileFilter := FileFilter, _
                 Title      := Title, _
                 MultiSelect := False)
    If result = False Then
        GetFilePath = ""
    Else
        GetFilePath = CStr(result)
    End If
End Function

' ---------------------------------------------------------------------------
' FileBaseName – extracts the filename without extension from a full path.
' ---------------------------------------------------------------------------
Private Function FileBaseName(filePath As String) As String
    Dim parts() As String
    parts = Split(filePath, "\")
    Dim fname As String
    fname = parts(UBound(parts))
    ' Remove extension
    Dim dotPos As Integer
    dotPos = InStrRev(fname, ".")
    If dotPos > 1 Then fname = Left(fname, dotPos - 1)
    FileBaseName = fname
End Function

' ---------------------------------------------------------------------------
' CleanSheetName – replaces characters illegal in Excel sheet names.
' ---------------------------------------------------------------------------
Private Function CleanSheetName(name As String) As String
    Dim illegal As String
    Dim i       As Integer
    illegal = "\/[]:*?"
    For i = 1 To Len(illegal)
        name = Replace(name, Mid(illegal, i, 1), "_")
    Next i
    CleanSheetName = name
End Function
