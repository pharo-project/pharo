An IndentingListItemMorph is a StringMorph that draws itself with an optional toggle at its left, as part of the display of the SimpleHierarchicalListMorph.

It will also display lines around the toggle depending on UITheme settings

Instance variables:

indentLevel <SmallInteger> 	the indent level, from 0 at the root and increasing by 1 at each level of the hierarchy.

isExpanded <Boolean>		true if this item is expanded (showing its children)

complexContents <ListItemWrapper>	an adapter wrapping my represented item that can answer its children, etc.
	
firstChild <IndentingListItemMorph|nil>	my first child, or nil if none
	
container <SimpleHierarchicalListMorph>	my container
	
nextSibling <IndentingListItemMorph|nil>	the next item in the linked list of siblings, or nil if none.

Contributed by Bob Arning as part of the ObjectExplorer package.
Don't blame him if it's not perfect.  We wanted to get it out for people to play with.