private
addClassCategoryBasedOn: aSelection
	| name package packageTag |

	packageTag := nil.
	package := aSelection package.
	name := UIManager default 
		request: ('New category: {1}-' format: { package name }) 
		initialAnswer: '' 
		title: 'Create a new category'.
	
	[ (self isValidPackageName: name)
		ifTrue: [ packageTag := package addClassTag: name ]
		ifFalse: [ self alertInvalidPackageName: name ] ]
	on: RPackageConflictError 
	do: [ :e | UIManager default inform: e messageText, '. You need to use promote/demote options instead.' ].
	
	^ packageTag