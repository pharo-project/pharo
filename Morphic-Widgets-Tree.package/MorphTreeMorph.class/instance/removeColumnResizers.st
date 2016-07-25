column handling
removeColumnResizers
	"Remove all column resizers"
	self removeAllMorphsIn: self columnResizers.
	self columnResizers do: [:r | r release].
	self columnResizers removeAll