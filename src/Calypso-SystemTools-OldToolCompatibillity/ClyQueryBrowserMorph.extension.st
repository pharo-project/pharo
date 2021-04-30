Extension { #name : #ClyQueryBrowserMorph }

{ #category : #'*Calypso-SystemTools-OldToolCompatibillity' }
ClyQueryBrowserMorph classSide >> beDefaultBrowser [
	<script>
	^ClyOldMessageBrowserAdapter beDefaultBrowser 
]
