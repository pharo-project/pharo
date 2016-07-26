private
process: anExample
	^ self process: anExample withoutLoopsDo: [ 
		self onProcess value: self value: anExample ]