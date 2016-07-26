private-formatting
formatArray: anArrayNode
	| current | 
	(anArrayNode statements reject: [ :each | each className = 'RBLiteralValueNode' ])
		ifEmpty: [ 
			anArrayNode statements
				do: [ :each | 
					current := each. 
					self visitNode: each ]
				separatedBy: [  
					(self isLineTooLong: current value asString)
						ifTrue: [ 
							codeStream nextPut: $..
							self newLine ]
						ifFalse: [ codeStream nextPutAll: ' . ' ] ] ]
		ifNotEmpty: [ self formatSequenceNodeStatementsFor: anArrayNode ]