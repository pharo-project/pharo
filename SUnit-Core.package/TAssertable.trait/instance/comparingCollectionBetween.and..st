private
comparingCollectionBetween: left and: right
	| additionalLeft additionalRight sortBlock|
	
	"use a very slow sort block"
	sortBlock := [ :a :b | a asString <= b asString ].
	additionalLeft := (left difference: right) sorted: sortBlock.
	additionalRight := (right difference: left) sorted: sortBlock. 
	
	^ String streamContents: [:stream |
		stream
			nextPutAll: 'Given Collections do not match. Got '; lf;
			tab; nextPutAll: 'left := '; print: left; nextPut: $.; lf;
			nextPutAll: ' instead of ';
			tab; nextPutAll: ' right :='; print: left; nextPut: $.; lf.
		left size = right size
			ifFalse: [ 
				stream 
					nextPutAll: 'Collection size does not match: left='; 
					print: left size;
					nextPutAll: ' vs. right=';
					print: right size; lf ].
		additionalLeft isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalLeft size;
					nextPutAll: ' additional element(s) in the left collection: ';
					tab; print: additionalLeft  ].
		additionalRight isEmpty
			ifFalse: [ 
				stream 
					nextPutAll: 'Got ';
					print: additionalRight size;
					nextPutAll: ' additional element(s) in the right collection: ';
					tab; print: additionalRight  ]]