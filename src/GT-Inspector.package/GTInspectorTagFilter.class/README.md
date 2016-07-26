GTInspectorTagFilter implements a filtering strategy for presentations based on tags.

A tag is defined as the parameter of the prama <gtInspectorTag:>. A method can have one or more tags.
The filter returns true for a method if that method contains at least a tag selected in the filter. To be taken into account during filtering a tag must be first added to the filter and the selected. Only tags that have been first added can be selected.  

If a method does not define any tags the filter threats that method as having a single tag stored in the #defaultTag instance variable. The default defaultTag is #custom. The #basic tag  groups the Raw and Meta presentations. Inspect 'GTInspectorTagFilter new allPragmasWithTag' to view all pragmas creating the tags.