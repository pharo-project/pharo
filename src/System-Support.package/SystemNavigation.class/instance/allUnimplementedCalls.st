query
allUnimplementedCalls
	"Answer an Array of each message that is sent by an expression in a  
	method but is not implemented by any object in the system."
	| aStream all |
	all := self allImplementedMessages.
	aStream := (Array new: 50) writeStream.
	self
		allBehaviorsDo: [:cl | cl
				selectorsAndMethodsDo: [:sel :meth |
					| secondStream ignored | 
					secondStream := (String new: 5) writeStream.
					ignored := (meth pragmaAt: #ignoreUnimplementedCalls:) 
						ifNotNil: [ :pragma | (pragma argumentAt: 1)  ] 
						ifNil: [ #() ].
					meth messages
						do: [:m | ((all includes: m) not and: [ (ignored includes: m) not])
								ifTrue: [ secondStream nextPutAll: m; space ]].
					secondStream position = 0
						ifFalse: [aStream nextPut: cl name , ' ' , sel , ' calls: ' , secondStream contents]]].
	^ aStream contents