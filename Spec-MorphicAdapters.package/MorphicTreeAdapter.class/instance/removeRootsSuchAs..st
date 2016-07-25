spec protocol
removeRootsSuchAs: aBlock

	self widgetDo: [ :w | w model rootNodes removeAllSuchThat: [ :node | aBlock value: node nodeModel ] ]