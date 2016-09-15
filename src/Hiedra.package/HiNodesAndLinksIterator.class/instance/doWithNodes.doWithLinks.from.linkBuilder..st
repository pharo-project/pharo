private
doWithNodes: nodesBlock doWithLinks: linksBlock from: initialValue linkBuilder: aLinkBuilder

	| pendingItems |
	pendingItems := Stack with: (self nodeAt: initialValue).
		
	[ pendingItems isEmpty ] whileFalse: [
		| currentItem |
		currentItem := pendingItems pop.

		(alreadyIteratedItems includes: currentItem) ifFalse: [
			alreadyIteratedItems add: currentItem.
			currentItem ifNode: nodesBlock ifLink: linksBlock ].
		
		currentItem isLink ifFalse: [
			(aLinkBuilder targetsOf: currentItem contents) do: [ :targetValue |
				| targetNode |
				targetNode := self nodeAt: targetValue.
				
				pendingItems
					push: (HiLink linkBuilder: aLinkBuilder origin: currentItem target: targetNode);
					push: targetNode ] ].
		].