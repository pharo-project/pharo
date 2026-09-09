Class {
	#name : 'CodeImporterCodeSnippetTest',
	#superclass : 'OCCompileCodeSnippetTest',
	#category : 'CodeImport-Tests',
	#package : 'CodeImport-Tests'
}

{ #category : 'tests' }
CodeImporterCodeSnippetTest >> testCodeImporter [

	| string importer class runBlock |
	"Code importer meed a plain expression or use a custom format"
	snippet source isAllSeparators ifTrue: [ ^ self skip ].
	[
	string := snippet isScripting
		          ifFalse: [
			          class := ChunkImportTestCase new importAClass.
			          '!{1} methodsFor: ''some protocol''!{2}' format: {
					          class name asString.
					          snippet source } ]
		          ifTrue: [ snippet source ].

	"Note: it might be possible that the snipped messes with the chuck format... to investigate"
	importer := CodeImporter fromString: string.
	importer parseChunks.

	self skipIf: #exec.

	"Importer should fail when faulty"
	snippet isFaulty ifTrue: [
		self should: [ importer evaluate ] raise: OCCompilationError, OCCodeError.
		snippet isScripting ifFalse: [ class removeFromSystem ].
		^ self ].

	"When not faulty, it's more complicated..."
	runBlock := [
	            | value |
	            value := importer evaluate.

	            snippet isScripting ifFalse: [
		            | method phonyArgs |
		            self assert: value isSymbol.
		            "Need to call the method, the importer added it to `class`, so retrieve it"
		            method := class >> value.
		            class removeFromSystem. "No more needed"
		            phonyArgs := (1 to: method numArgs) asArray.
		            value := nil withArgs: phonyArgs executeMethod: method ].

	            value ].

	self testExecuteBlock: runBlock ] ensure: [ self packageOrganizer removePackage: ChunkImportTestCase new packageNameForTests ]
]
