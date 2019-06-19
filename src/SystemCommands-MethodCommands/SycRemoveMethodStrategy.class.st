"
I am a strategy on how remove methods.
You can see my subclasses when you try remove method in the browser which is still in use.
Usualy you see four options what to do:
- just remove 
- remove and show senders
- do not remove and show senders
- cancel  

My subclasses reprsent this choices using following method: 

- removeMethods: methods 

And they implement supporting methods to be shown in this dialog nicely: 

- orderForBrowser 
Strategy selection dialog orders collected strategy using this message. 

- userRequestString 
It is a string which should be displayed in strategy selection dialog.
"
Class {
	#name : #SycRemoveMethodStrategy,
	#superclass : #Object,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #execution }
SycRemoveMethodStrategy >> removeMethods: methods [
	self subclassResponsibility 
]

{ #category : #execution }
SycRemoveMethodStrategy >> userRequestString [
	self subclassResponsibility 
]
