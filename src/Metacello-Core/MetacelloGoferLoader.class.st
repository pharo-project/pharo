Class {
	#name : 'MetacelloGoferLoader',
	#superclass : 'Object',
	#category : 'Metacello-Core-Gofer',
	#package : 'Metacello-Core',
	#tag : 'Gofer'
}

{ #category : 'as yet unclassified' }
MetacelloGoferLoader >> loadPackageDirective: aMetacelloPackageLoadDirective [

	| resolvedReference goferLoad repo |
	resolvedReference := aMetacelloPackageLoadDirective externalReference.
	goferLoad := MetacelloGoferLoad on: Gofer new.
	goferLoad addResolved: resolvedReference.
	goferLoad execute.
	repo := resolvedReference repository.
	resolvedReference workingCopy repositoryGroup addRepository:
		resolvedReference repository.
	MetacelloNotification signal:
		'Loaded -> ' , resolvedReference name , ' --- '
		, repo repositoryDescription , ' --- '
		, resolvedReference repository description
]
