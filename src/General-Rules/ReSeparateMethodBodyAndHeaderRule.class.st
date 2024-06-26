"
this rule verify if there is an empty line between the method signature and the method body. it ensure a better reading of the method.
"
Class {
	#name : 'ReSeparateMethodBodyAndHeaderRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'testing - interest' }
ReSeparateMethodBodyAndHeaderRule class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReSeparateMethodBodyAndHeaderRule class >> group [

	^ 'Formatting'
]

{ #category : 'accessing' }
ReSeparateMethodBodyAndHeaderRule class >> rationale [

	^ 'Make sure that the reader can identify the beginning of the method body by separating the method signature and comments from the method body by an empty line.'
]

{ #category : 'accessing' }
ReSeparateMethodBodyAndHeaderRule class >> ruleName [

	^ 'Separate signature and comments from method body'
]

{ #category : 'helpers' }
ReSeparateMethodBodyAndHeaderRule >> anchorFor: aNode [

	| endOfFirstLine |
	endOfFirstLine := aNode methodNode body hasTemporaries
		ifTrue: [ aNode methodNode body rightBar + 1 ]
		ifFalse:[ aNode methodNode body statements first stop + 1 ].

	^ ReIntervalSourceAnchor
			entity: aNode methodNode body
			interval: (Interval from: aNode methodNode body start to: endOfFirstLine)
]

{ #category : 'running' }
ReSeparateMethodBodyAndHeaderRule >> basicCheck: anEntity [

	| endOfHeader startOfBody |
	endOfHeader := anEntity methodNode arguments
		ifEmpty: [ anEntity methodNode selector keywords first size + 1 ]
		ifNotEmpty: [
			| lastArgument |
			lastArgument := anEntity methodNode arguments last.
			lastArgument stop ].
	startOfBody := anEntity methodNode body start.
	startOfBody < endOfHeader ifTrue:[ ^ false ].

	anEntity methodNode comments notEmpty ifTrue:[ 
		| headerComments |
		headerComments :=  anEntity methodNode comments first.
		headerComments stop < startOfBody
			ifTrue:[
				"Check if there is no empty line between the selector and the comment."
				(self emptyLineIn: anEntity methodNode sourceCode between: endOfHeader and: headerComments start - 1) 
					ifTrue: [ ^ true ].
				endOfHeader := endOfHeader max: headerComments stop ] ].
	"Check if there is an empty line between the end of the header (possibly including comment) and the start of the method body."
	^ (self emptyLineIn: anEntity methodNode sourceCode between: endOfHeader and: startOfBody - 1) not
]

{ #category : 'private' }
ReSeparateMethodBodyAndHeaderRule >> emptyLineIn: aString between: start and: end [
	"Check if there are two consecutive <cr> characters only separated by whitespace between start and end"
	
	| part |
	part := aString copyFrom: start to: end - 1.
	^ part matchesRegex: '.*\r+\s*\r+.*'
]
