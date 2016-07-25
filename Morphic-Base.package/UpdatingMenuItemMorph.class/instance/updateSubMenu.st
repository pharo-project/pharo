private
updateSubMenu

	target ifNotNil: [ subMenu := (target perform: subMenuSelector) ]