accessing
reportSummary

	^ 'Author: {1}

{2}' format: { report at: #author. (Character cr join: (report at: #payload)) }