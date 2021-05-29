"
I am an abstract visitor. I provide a default implementation of all visit messages that does not descend automatically into children of the visited graph. Subclasses should override all my messages in appropriate ways to visit the nodes they need.
"
Class {
	#name : #PRVisitor,
	#superclass : #PRBasicObject,
	#category : #'Pillar-Core-Visitor'
}

{ #category : #visiting }
PRVisitor >> start: anObject [
	^ self visit: anObject
]

{ #category : #visiting }
PRVisitor >> visit: anObject [
	^ anObject acceptDecorated: self
]

{ #category : #visiting }
PRVisitor >> visitAll: aCollection [
	^aCollection collect: [ :each | self visit: each ]
]

{ #category : #visiting }
PRVisitor >> visitAll: aCollection separatedBy: aBlock [
	aCollection do: [ :each | self visit: each ] separatedBy: aBlock
]

{ #category : #'visiting - document' }
PRVisitor >> visitAnchor: anObject [
	^ self visitDocumentItem: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitAnnotatedParagraph: aDocument [
	^ self visitParagraph: aDocument
]

{ #category : #'visiting - annotations' }
PRVisitor >> visitAnnotation: anAnnotation [
	"Visiting an annotation means that we visit the textual representation of an annotation. Such annotations will be transformed into annotation object. The corresponding visit method will be invoked on such object : visitCitationAnnotation vs visitCitation."
	^ self visitDocumentGroup: anAnnotation
]

{ #category : #'visiting - annotations' }
PRVisitor >> visitBeginEnvironmentAnnotation: aBeginEnvironmentAnnotation [
	"Visiting an annotation means that we visit the textual representation of an annotation. Such annotations will be transformed into annotation object. The corresponding visit method will be invoked on such object : visitBeginEnvironmentAnnotation vs visitBeginEnvironment."
	^ self visitAnnotation: aBeginEnvironmentAnnotation
]

{ #category : #'visiting - document' }
PRVisitor >> visitBoldFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitCodeblock: aCodeBlock [ 
	^ self visitText: aCodeBlock
]

{ #category : #'visiting - document' }
PRVisitor >> visitCommentedLine: aCommentedLine [

]

{ #category : #'visiting - document' }
PRVisitor >> visitDataItem: anObject [
	^ self visitListItem: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitDefinitionList: anObject [
	^ self visitList: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitDocument: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitDocumentGroup: anObject [
	^ self visitAll: anObject children
]

{ #category : #'visiting - document' }
PRVisitor >> visitDocumentItem: anObject [
]

{ #category : #'visiting - document' }
PRVisitor >> visitEmptyParagraph: anObject [
	<ignoreForCoverage "This method is ignored for coverage because visiting an empty paragraph is typically very much different from visiting a paragraph.">
	
	^ self visitParagraph: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitEnvironment: anEnvironment [
	"An environment object has been created based on the begin and end annotations.
	My subclasses may want to introduce a preAction and postAction triggering. By default just visit all children."
	self visitParameters: anEnvironment parameters.
	^ self visitDocumentGroup: anEnvironment
]

{ #category : #'visiting - document' }
PRVisitor >> visitExternalLink: anObject [
	^ self visitLink: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitFigure: anObject [
	^ self visitReference: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitFormat: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitHeader: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitHorizontalRule: anObject [
	^ self visitDocumentItem: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitInternalLink: anObject [
	^ self visitLink: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitItalicFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitLineBreak: anObject [
	<ignoreForCoverage "This method is ignored for coverage because visiting a line break is typically very much different from visiting a standard document item.">
	^ self visitDocumentItem: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitLink: anObject [
	^ self visitReference: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitList: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitListItem: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitMailLink: anObject [
	^ self visitExternalLink: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitMathEnvironment: aMathEnvironment [
	
	self visitParameters: aMathEnvironment parameters.
	^ self visitDocumentGroup: aMathEnvironment
]

{ #category : #'visiting - document' }
PRVisitor >> visitMonospaceFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitOrderedList: anObject [
	^ self visitList: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitParagraph: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - parameters' }
PRVisitor >> visitParameter: anObject [
	^ self visitDocumentGroup: anObject 
]

{ #category : #'visiting - parameters' }
PRVisitor >> visitParameterKey: anObject [
	^ self visitDocumentItem: anObject 
]

{ #category : #'visiting - parameters' }
PRVisitor >> visitParameterValue: anObject [
	^ self visitDocumentItem: anObject 
]

{ #category : #'visiting - parameters' }
PRVisitor >> visitParameters: anObject [
	"To me this cannot work since an object is a dictionary and not a document group
	I let it for now because I do not have all pillar loaded but I'm suspicious."
	^ self visitDocumentGroup: anObject 
]

{ #category : #'visiting - document' }
PRVisitor >> visitPreformatted: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitRaw: aRaw [
	^ self visitText: aRaw
]

{ #category : #'visiting - document' }
PRVisitor >> visitReference: anObject [
	^ self visitDocumentGroup: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitSection: aSection [
	^ self visitDocumentGroup: aSection
]

{ #category : #'visiting - document' }
PRVisitor >> visitStrikethroughFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitSubscriptFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitSuperscriptFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitTermItem: anObject [
	^ self visitListItem: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitText: anObject [
	^ self visitDocumentItem: anObject
]

{ #category : #'visiting - annotations' }
PRVisitor >> visitUndefinedAnnotation: aUndefinedAnnotation [
	"Visiting an annotation means that we visit the textual representation of an annotation. Such annotations will be transformed into annotation object. The corresponding visit method will be invoked on such object : visitFootnoteAnnotation vs visitFootnote."
	^ self visitAnnotation: aUndefinedAnnotation
]

{ #category : #'visiting - document' }
PRVisitor >> visitUnderlineFormat: anObject [
	^ self visitFormat: anObject
]

{ #category : #'visiting - document' }
PRVisitor >> visitUnorderedList: anObject [
	^ self visitList: anObject
]
