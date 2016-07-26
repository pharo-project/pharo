private
do: aBlock on: aCollection displaying: aString

	| job |
	aCollection ifEmpty: [ ^ self ].
	job := [ :bar | 
		aCollection do: [ :element | 
			aBlock value: element.
			bar increment ] ] asJob.
	job
		title: aString;
		min: 0;
		max: aCollection size;
		run.