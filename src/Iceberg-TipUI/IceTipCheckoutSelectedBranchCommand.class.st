"
Command that checks out the selected branch
"
Class {
	#name : #IceTipCheckoutSelectedBranchCommand,
	#superclass : #IceTipRepositoryCommand,
	#instVars : [
		'branchModel'
	],
	#category : #'Iceberg-TipUI-Commands'
}

{ #category : #testing }
IceTipCheckoutSelectedBranchCommand class >> actsOnBranch: aBranchModel [
	^ true
]

{ #category : #execution }
IceTipCheckoutSelectedBranchCommand class >> browserContextActivation [
    <classAnnotation>
    
    ^ CmdContextMenuActivation byRootGroupItemOrder: 10 for: IceTipRepositoryContext
]

{ #category : #execution }
IceTipCheckoutSelectedBranchCommand class >> canBeExecutedInContext: aToolContext [
    ^ aToolContext item canActivateCommand: self
]

{ #category : #accessing }
IceTipCheckoutSelectedBranchCommand class >> defaultHelp [
	^ 'Checkout this branch'
]

{ #category : #accessing }
IceTipCheckoutSelectedBranchCommand class >> defaultMenuIconName [
	^ #branch
]

{ #category : #accessing }
IceTipCheckoutSelectedBranchCommand class >> defaultMenuItemName [
	^ 'Checkout branch'
]

{ #category : #execution }
IceTipCheckoutSelectedBranchCommand >> execute [
	branchModel previewCheckout
]

{ #category : #execution }
IceTipCheckoutSelectedBranchCommand >> readParametersFromContext: aToolContext [
	super readParametersFromContext: aToolContext.
	branchModel := aToolContext item.
]
