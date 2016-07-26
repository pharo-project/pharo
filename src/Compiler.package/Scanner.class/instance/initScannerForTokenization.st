initialize-release
initScannerForTokenization
	"Use a version of typeTable that doesn't raise xIllegal when enocuntering an :="
	| underscoreIndex |
	underscoreIndex := typeTable indexOf: #xUnderscore ifAbsent: [^self].
	typeTable := typeTable copy.
	typeTable at: underscoreIndex put: #xUnderscoreForTokenization