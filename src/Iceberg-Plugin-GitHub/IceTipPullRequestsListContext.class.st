"
Description
--------------------

I am a simple context for the github pull requests browser.
"
Class {
	#name : #IceTipPullRequestsListContext,
	#superclass : #IceTipContext,
	#instVars : [
		'item'
	],
	#category : #'Iceberg-Plugin-GitHub-Commands'
}

{ #category : #accessing }
IceTipPullRequestsListContext >> item [
	^ item
]

{ #category : #accessing }
IceTipPullRequestsListContext >> item: anObject [
	item := anObject
]
