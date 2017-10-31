comparing
compareClass: oldClass with: aBuilder
	| aComparer changes |
	aComparer := self new.
	aComparer builder: aBuilder.
	
	changes := aComparer compareClass.
	
	changes do:[:e | aBuilder addChange: e].