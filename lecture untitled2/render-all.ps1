param([string]$RBin = 'C:/Program Files/R/R-4.3.3/bin')
$ErrorActionPreference = 'Stop'
if (Test-Path -LiteralPath $RBin) { $env:QUARTO_R = $RBin }
foreach ($lectureNumber in 4..6) {
    Push-Location (Join-Path $PSScriptRoot "lecture $lectureNumber")
    try {
        & quarto render
        if ($LASTEXITCODE -ne 0) { throw "Lecture $lectureNumber tutorial render failed." }
        Push-Location slides
        try {
            & quarto render
            if ($LASTEXITCODE -ne 0) { throw "Lecture $lectureNumber slide render failed." }
        } finally { Pop-Location }
    } finally { Pop-Location }
}
Write-Output 'All tutorials and slide decks are rendered. Open index.html.'
