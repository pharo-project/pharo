Class {
	#name : #SpMorphicTreeAdapter,
	#superclass : #SpMorphicTreeTableAdapter,
	#category : #'Spec2-Adapters-Morphic-Table'
}

{ #category : #factory }
SpMorphicTreeAdapter >> columns [
	
	^ #()
]

{ #category : #factory }
SpMorphicTreeAdapter >> defaultColumn [
	| stringColumn |

	stringColumn := SpStringTableColumn evaluated: self presenter display.
	^ self presenter displayIcon 
		ifNotNil: [ :iconBlock |
			SpCompositeTableColumn new 
				addColumn: (SpImageTableColumn evaluated: iconBlock) beNotExpandable;
				addColumn: stringColumn;
				yourself ]
		ifNil: [ stringColumn ]
]

{ #category : #factory }
SpMorphicTreeAdapter >> ensureAtLeastOneColumnIn: tableMorph [

	tableMorph addColumn: (self newColumnFrom: self defaultColumn)
]

{ #category : #factory }
SpMorphicTreeAdapter >> registerColumnsChangedEventTo: tableMorph [
]
