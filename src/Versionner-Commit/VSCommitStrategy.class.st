"
I define the collection strategy for the commit: 

- should I commit dirty packages?
- should I commit projects?
- should I include nested projects?

By default, strategy is complete (yes, yes and yes), but users can change if it convinces them.

I'm included in a commit unit and I should not  be accessed outside it. 
"
Class {
	#name : #VSCommitStrategy,
	#superclass : #Object,
	#instVars : [
		'shouldCommitProjects',
		'shouldCommitNestedProjects',
		'shouldNestProjects',
		'shouldCommitDirtyPackages'
	],
	#category : #'Versionner-Commit'
}

{ #category : #initialization }
VSCommitStrategy >> initialize [ 
	super initialize.
	shouldCommitDirtyPackages := true.
	shouldCommitProjects := true.
	shouldCommitNestedProjects := false.
	shouldNestProjects := true.
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitStrategy >> options [
	^ {  
	{ 'Nest projects?'. 				#shouldNestProjects}.
	{ 'Commit projects?'. 			#shouldCommitProjects}.
	{ 'Commit all projects (even those not dirty)?'. 	#shouldCommitNestedProjects }.
	{ 'Commit dirty packages?'. 	#shouldCommitDirtyPackages} 
	}
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitDirtyPackages [
	^ shouldCommitDirtyPackages
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitDirtyPackages: anObject [
	shouldCommitDirtyPackages := anObject
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitNestedProjects [
	^ shouldCommitNestedProjects
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitNestedProjects: anObject [
	shouldCommitNestedProjects := anObject
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitProjects [
	^ shouldCommitProjects
]

{ #category : #accessing }
VSCommitStrategy >> shouldCommitProjects: anObject [
	shouldCommitProjects := anObject
]

{ #category : #accessing }
VSCommitStrategy >> shouldNestProjects [
	^ shouldNestProjects
]

{ #category : #accessing }
VSCommitStrategy >> shouldNestProjects: anObject [
	shouldNestProjects := anObject
]
