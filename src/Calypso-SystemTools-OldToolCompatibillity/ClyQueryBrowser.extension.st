Extension { #name : #ClyQueryBrowser }

{ #category : #'*Calypso-SystemTools-OldToolCompatibillity' }
ClyQueryBrowser class >> beDefaultBrowser [
	<script>
	^ClyOldMessageBrowserAdapter beDefaultBrowser 
]
