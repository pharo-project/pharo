"
SearchFacade is a facade for creating searching dialog window
"
Class {
	#name : #SearchFacade,
	#superclass : #Object,
	#category : #'Tool-Base'
}

{ #category : #'message search' }
SearchFacade class >> allMessageSearchFor: aClass [

	^ self messageSearchIn: aClass allMethods
]

{ #category : #'class search' }
SearchFacade class >> classSearch [

	^ self classSearchInEnvironment: self environment
]

{ #category : #'class search' }
SearchFacade class >> classSearchIn: classes [
	^ ListDialogWindow new
		getList: [ :r | classes select: [ :e | r search: e name ] ];
		displayBlock: [ :e | e name ];
		title: 'Class search' translated;
		yourself
]

{ #category : #'class search' }
SearchFacade class >> classSearchInEnvironment: anEnvironment [
	| classes |
	classes :=  anEnvironment classesAndTraits sorted: [ :a :b| a name <= b name ].
	^ self classSearchIn: classes.
]

{ #category : #'class search' }
SearchFacade class >> hierarchySearchFor: aClass [
	| classes |
	classes := aClass allSuperclasses reverse, (aClass allSubclasses sorted: [ :a :b| a name <= b name ]).
	^ self classSearchIn: classes
]

{ #category : #'message search' }
SearchFacade class >> messageSearchFor: aClass [

	^ self messageSearchIn: aClass methods
]

{ #category : #'message search' }
SearchFacade class >> messageSearchIn: aCollection [
	| rawList |
	rawList := aCollection sorted: [ :a :b | a selector <= b selector ].
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e selector ] ];
		displayBlock: [ :e | e selector ];
		title: 'Message search' translated;
		yourself
]

{ #category : #'package search' }
SearchFacade class >> packageSearch [
				
	^ self packageSearchIn: self environment
]

{ #category : #'package search' }
SearchFacade class >> packageSearchIn: anEnvironment [
	| rawList |
	rawList := anEnvironment organization categories sort.
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		title: 'Package search' translated;
		yourself
]

{ #category : #'protocol search' }
SearchFacade class >> protocolSearchIn: aCollection [
	^ ListDialogWindow new
		getList: [ :r | aCollection sorted select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		title: 'Protocol search' translated;
		yourself
]

{ #category : #'rpackage search' }
SearchFacade class >> rPackageSearch [
				
	^ self rPackageSearchIn: RPackageOrganizer default
]

{ #category : #'rpackage search' }
SearchFacade class >> rPackageSearchIn: anOrganizer [
	| rawList |
	rawList := anOrganizer packages sorted: [ :a :b | a name <= b name ].
	^ ListDialogWindow new
		getList: [ :r | rawList select: [ :e | r search: e name ] ];
		displayBlock: [ :e | e name ];
		browseBlock: [ :answer | Smalltalk tools browser openOnPackage: answer ];
		title: 'Package search' translated;
		yourself
]

{ #category : #'class search' }
SearchFacade class >> subclassSearchFor: aClass [
	| classes |
	classes := aClass allSubclasses sorted: [ :a :b| a name <= b name ].
	^ self classSearchIn: classes
]

{ #category : #'class search' }
SearchFacade class >> superclassSearchFor: aClass [

	^ self classSearchIn: aClass allSuperclasses reverse
]
