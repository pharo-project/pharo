Extension { #name : #IceTipInspectCommand }

{ #category : #'*Iceberg-Plugin-GitHub' }
IceTipInspectCommand class >> browserPullRequestListContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation byRootGroupItemFor: IceTipPullRequestsContext
]
