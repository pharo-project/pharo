Extension { #name : 'ClyQueryBrowserMorph' }

{ #category : '*Calypso-SystemTools-OldToolCompatibillity' }
ClyQueryBrowserMorph class >> beDefaultBrowser [
	<script>
	^ClyOldMessageBrowserAdapter beDefaultBrowser
]
