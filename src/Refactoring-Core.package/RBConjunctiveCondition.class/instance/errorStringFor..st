private
errorStringFor: aBoolean 
	^aBoolean
		ifTrue: [self neitherFailed]
		ifFalse: [self perform: failed]