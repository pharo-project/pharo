"
I'm a context for the project managet tool.
I'm to be used with project lists.
"
Class {
	#name : #IceTipRepositoryListContext,
	#superclass : #IceTipContext,
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #accessing }
IceTipRepositoryListContext >> item [
	
	^ self repositoryModel
]
