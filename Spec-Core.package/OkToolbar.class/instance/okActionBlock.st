private
okActionBlock

	^ [ okAction value value == false
		ifFalse: [ owner 
			ifNil: [ self delete ] 
			ifNotNil: [ owner delete ] ] ]