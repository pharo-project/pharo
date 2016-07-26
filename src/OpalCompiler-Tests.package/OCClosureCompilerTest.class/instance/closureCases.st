source
closureCases
	^#(
'| n |
n := 1.
^n + n'

'[:c :s| | mn |
mn := Compiler new
		compile: (c sourceCodeAt: s)
		in: c
		notifying: nil
		ifFail: [self halt].
mn generate: #(0 0 0 0).
{mn blockExtentsToTempsMap.
  mn encoder schematicTempNames}]
			value: AbstractInstructionTests
			value: #runBinaryConditionalJumps:'

'inject: thisValue into: binaryBlock
	| nextValue |
	nextValue := thisValue.
	self do: [:each | nextValue := binaryBlock value: nextValue value: each].
	^nextValue'

'mapFromBlockStartsIn: aMethod toTempVarsFrom: schematicTempNamesString constructor: aDecompilerConstructor
	| map |
	map := aMethod
				mapFromBlockKeys: aMethod startpcsToBlockExtents keys asSortedCollection
				toSchematicTemps: schematicTempNamesString.
	map keysAndValuesDo:
		[:startpc :tempNameTupleVector| | subMap tempVector numTemps |
		subMap := Dictionary new.
		tempNameTupleVector do:
			[:tuple|
			tuple last isArray
				ifTrue:
					[subMap at: tuple last first put: tuple last last.
					 numTemps := tuple last first]
				ifFalse:
					[numTemps := tuple last]].
		tempVector := Array new: numTemps.
		subMap keysAndValuesDo:
			[:index :size|
			tempVector at: index put: (Array new: size)].
		tempNameTupleVector do:
			[:tuple| | itv |
			tuple last isArray
				ifTrue:
					[itv := tempVector at: tuple last first.
					 itv at: tuple last last
						put: (aDecompilerConstructor
								codeTemp: tuple last last - 1
								named: tuple first)]
				ifFalse:
					[tempVector
						at: tuple last
						put: (aDecompilerConstructor
								codeTemp: tuple last - 1
								named: tuple first)]].
		subMap keysAndValuesDo:
			[:index :size|
			tempVector
				at: index
				put: (aDecompilerConstructor
						codeRemoteTemp: index
						remoteTemps: (tempVector at: index))].
		map at: startpc put: tempVector].
	^map'

 'gnuifyFrom: inFileStream to: outFileStream

	| inData beforeInterpret inInterpret inInterpretVars beforePrimitiveResponse inPrimitiveResponse |

	inData := inFileStream upToEnd withSqueakLineEndings.
	inFileStream close.

	outFileStream
		nextPutAll: ''/* This file has been post-processed for GNU C */'';
		cr; cr; cr.

	beforeInterpret := true.    "whether we are before the beginning of interpret()"
	inInterpret := false.     "whether we are in the middle of interpret"
	inInterpretVars := false.    "whether we are in the variables of interpret"
	beforePrimitiveResponse := true.  "whether we are before the beginning of primitiveResponse()"
	inPrimitiveResponse := false.   "whether we are inside of primitiveResponse"
	''Gnuifying''
		displayProgressAt: Sensor cursorPoint
		from: 1 to: (inData occurrencesOf: Character cr)
		during:
			[:bar | | lineNumber |
			lineNumber := 0.
			inData linesDo:
				[ :inLine | | outLine extraOutLine caseLabel |
				bar value: (lineNumber := lineNumber + 1).
				outLine := inLine. 	"print out one line for each input line; by default, print out the line that was input, but some rules modify it"
				extraOutLine := nil.   "occasionally print a second output line..."
				beforeInterpret ifTrue: [
					inLine = ''#include "sq.h"'' ifTrue: [
						outLine := ''#include "sqGnu.h"'' ].
					inLine = ''interpret(void) {'' ifTrue: [
						"reached the beginning of interpret"
						beforeInterpret := false.
						inInterpret := true.
						inInterpretVars := true ] ]
				ifFalse: [
				inInterpretVars ifTrue: [
					(inLine findString: ''register struct foo * foo = &fum;'') > 0 ifTrue: [
						outLine := ''register struct foo * foo FOO_REG = &fum;'' ].
					(inLine findString: '' localIP;'') > 0 ifTrue: [
						outLine := ''    char* localIP IP_REG;'' ].
					(inLine findString: '' localFP;'') > 0 ifTrue: [
						outLine := ''    char* localFP FP_REG;'' ].
					(inLine findString: '' localSP;'') > 0 ifTrue: [
						outLine := ''    char* localSP SP_REG;'' ].
					(inLine findString: '' currentBytecode;'') > 0 ifTrue: [
						outLine := ''    sqInt currentBytecode CB_REG;'' ].
					inLine isEmpty ifTrue: [
						"reached end of variables"
						inInterpretVars := false.
						outLine := ''    JUMP_TABLE;''.
						extraOutLine := inLine ] ]
				ifFalse: [
				inInterpret ifTrue: [
					"working inside interpret(); translate the switch statement"
					(inLine beginsWith: ''		case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''		CASE('', caseLabel, '')'' ].
					inLine = ''			break;'' ifTrue: [
						outLine := ''			BREAK;'' ].
					inLine = ''}'' ifTrue: [
						"all finished with interpret()"
						inInterpret := false ] ]
				ifFalse: [
				beforePrimitiveResponse ifTrue: [
					(inLine beginsWith: ''primitiveResponse('') ifTrue: [
						"into primitiveResponse we go"
						beforePrimitiveResponse := false.
						inPrimitiveResponse := true.
						extraOutLine := ''    PRIM_TABLE;'' ] ]
				ifFalse: [
				inPrimitiveResponse ifTrue: [
					inLine = ''	switch (primitiveIndex) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					inLine = ''	switch (GIV(primitiveIndex)) {'' ifTrue: [
						extraOutLine := outLine.
						outLine := ''	PRIM_DISPATCH;'' ].
					(inLine beginsWith: ''	case '') ifTrue: [
						caseLabel := (inLine findTokens: ''	 :'') second.
						outLine := ''	CASE('', caseLabel, '')'' ].
					inLine = ''}'' ifTrue: [
						inPrimitiveResponse := false ] ]
				] ] ] ].

				outFileStream nextPutAll: outLine; cr.
				extraOutLine ifNotNil: [
					outFileStream nextPutAll: extraOutLine; cr ]]].

	outFileStream close' )