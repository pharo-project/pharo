enumeration
specListDo: aBlock

	self list do: [:member |  aBlock value: member spec ]