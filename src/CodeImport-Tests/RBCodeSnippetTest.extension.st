Extension { #name : #RBCodeSnippetTest }

{ #category : #'*CodeImport-Tests' }
RBCodeSnippetTest >> testCodeImporter [

	| string importer class runBlock value |
	"Code importer meed a plain expression or use a custom format"
	snippet source isAllSeparators ifTrue: [ ^ self skip ].
	string := snippet isMethod
		          ifTrue: [
			          class := ChunkImportTestCase new importAClass.
			          '!{1} methodsFor: ''some protocol''!{2}' format: {
					          class name asString.
					          snippet source } ]
		          ifFalse: [ snippet source ].

	"Note: it might be possible that the snipped messes with the chuck format... to investigate"
	importer := CodeImporter fromString: string.
	importer parseChunks.

	self skipIf: #exec.

	"Importer should fail when faulty"
	snippet isFaulty ifTrue: [
		self should: [ importer evaluate ] raise: CodeError.
		snippet isMethod ifTrue: [ class removeFromSystem ].
		^ self ].

	"When not faulty, it's more complicated..."
	runBlock := [
	            value := importer evaluate.

	            snippet isMethod ifTrue: [
		            | method phonyArgs |
		            self assert: value isSymbol.
		            "Need to call the method, the importer added it to `class`, so retrieve it"
		            method := class >> value.
		            class removeFromSystem. "No more needed"
		            phonyArgs := (1 to: method numArgs) asArray.
		            value := nil withArgs: phonyArgs executeMethod: method ].

	            "Need to execute the block (see testExecute for rationale)"
	            value isBlock ifTrue: [
		            | phonyBlockArgs |
		            phonyBlockArgs := (1 to: value numArgs) asArray.
		            value := value valueWithArguments: phonyBlockArgs ] ].

	snippet messageNotUnderstood ifNotNil: [ :mnu |
		runBlock onDNU: mnu do: [ ^ self ].
		^ self signalFailure: 'Should have raised MNU ' , mnu ].

	snippet raise ifNotNil: [ :r | ^ self should: runBlock raise: r ].

	self shouldnt: runBlock raise: CodeError.
	self assert: value equals: snippet value
]
