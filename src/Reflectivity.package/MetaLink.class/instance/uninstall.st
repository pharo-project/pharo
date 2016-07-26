installing
uninstall
	nodes do: [:node | node removeLink: self].
	nodes removeAll.