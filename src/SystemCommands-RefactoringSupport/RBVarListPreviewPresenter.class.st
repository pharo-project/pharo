"
I am preview to select variables (Class Variable) 
"
Class {
	#name : #RBVarListPreviewPresenter,
	#superclass : #RBListPreviewPresenter,
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #'instance creation' }
RBVarListPreviewPresenter class >> example [
	<script>
	^ (self label: 'example ...' 
		withItems: {(ClassVariable named: #a) . (ClassVariable named: #b) . (ClassVariable named: #z) } 
		selecting: {(ClassVariable named: #z) . (ClassVariable named: #a) } )
]

{ #category : #specs }
RBVarListPreviewPresenter class >> title [ 
	^ 'Class variables'
]

{ #category : #attributes }
RBVarListPreviewPresenter >> columnName [
	^ 'Variables'
]

{ #category : #attributes }
RBVarListPreviewPresenter >> columnSelector [ 
	^ #name
]
