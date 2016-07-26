accessing
onDo
	^ onDo ifNil: [ onDo := [ :anItemBlock :aContext | " do nothing " ] ]