"
I am an item within a *PRList*. It means that my subclass should only be elements having 
a textual representation within Pillar (- -- #).
If you need to extend Pillar to support new kind of item (for example bibitem) which do not have
a pillar syntax support do not subclass from this class. 
"
Class {
	#name : #PRListItem,
	#superclass : #PRDocumentGroup,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRListItem >> accept: aVisitor [
	^ aVisitor visitListItem: self
]
