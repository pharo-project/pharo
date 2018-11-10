"
I'm a panel to peform the checkout of an existing branch.
"
Class {
	#name : #IceTipCheckoutBranchPanel,
	#superclass : #IceTipExistingBranchPanel,
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #actions }
IceTipCheckoutBranchPanel >> doAccept [
	self validate.
	self selectedBranch previewCheckout
]
