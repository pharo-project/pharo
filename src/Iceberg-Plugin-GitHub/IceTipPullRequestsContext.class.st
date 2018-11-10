"
Description
--------------------

I am a simple context for the pull request review browser.
"
Class {
	#name : #IceTipPullRequestsContext,
	#superclass : #IceTipContext,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceTipPullRequestsContext >> item [
	^ item
]

{ #category : #accessing }
IceTipPullRequestsContext >> item: anObject [
	item := anObject
]
