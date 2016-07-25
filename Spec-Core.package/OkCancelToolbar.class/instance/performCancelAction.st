api
performCancelAction

	^ cancelAction value value == false
		ifFalse: [ 
			cancelled value: true.
			owner ifNil: [ self delete ] ifNotNil: [ owner delete ] ]