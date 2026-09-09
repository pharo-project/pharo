Extension { #name : 'Behavior' }

{ #category : '*Deprecated14' }
Behavior >> compile: sourceCode classified: protcol notifying: requestor [
	"Return the selector of the compiled method"

	self deprecated: 'Use #compile:classified:withStamp:notifying:logSource: if you need the old style requestor'.
	^ self
		  compile: sourceCode
		  classified: protcol
		  withStamp: DateAndTime now
		  notifying: requestor
		  logSource: self shouldLogSource
]

{ #category : '*Deprecated14' }
Behavior >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor [
	"Return the selector of the compiled method"

	self deprecated: 'Use #compile:classified:withStamp:notifying:logSource: if you need the old style requestor'.
	^ self
		  compile: sourceCode
		  classified: protocol
		  withStamp: changeStamp
		  notifying: requestor
		  logSource: self shouldLogSource
]

{ #category : '*Deprecated14' }
Behavior >> compile: code notifying: requestor [
	"Compile the argument, code, as source code in the context of the
	receiver and insEtall the result in the receiver's method dictionary. The
	second argument, requestor, is to be notified if an error occurs. The
	argument code is either a string or an object that converts to a string or
	a PositionableStream. This method also saves the source code."

	self deprecated: 'Use #compile:classified:withStamp:notifying:logSource: if you need the old style requestor'.
	^ self
		  compile: code
		  classified: nil
		  withStamp: DateAndTime now
		  notifying: requestor
		  logSource: self shouldLogSource
]
