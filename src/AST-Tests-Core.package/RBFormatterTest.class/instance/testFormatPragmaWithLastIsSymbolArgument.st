testing
testFormatPragmaWithLastIsSymbolArgument
	| inputSource tree outputSource |
	"formatter should not create invalid code by removing whitespace between #= and >"
	inputSource := 'foo  <selector: #= > ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification.
	
	"already worked and still should for non-symbol arguments"
	inputSource := 'foo  <selector: 0 > ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification.
	
	"already worked and should still work for pragmas without arguments"
	inputSource := 'foo  <selector> ^ self'.
	tree := RBParser parseMethod: inputSource.
	outputSource := self formatterClass new format: tree.
	self shouldnt: [RBParser parseMethod: outputSource] raise: SyntaxErrorNotification.

