event subscriptions
critiqueBanned: aReCritiqueBanned
	| baner |
	baner := aReCritiqueBanned entityBanLevel.
	
	baner class = RPackage
		ifTrue: [ self removeManagerFor: baner ]