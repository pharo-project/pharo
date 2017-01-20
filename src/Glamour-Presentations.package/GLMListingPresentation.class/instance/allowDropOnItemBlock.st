accessing
allowDropOnItemBlock
	^ allowDropOnItemBlock ifNil: [ allowDropOnItemBlock := [ false ] ]