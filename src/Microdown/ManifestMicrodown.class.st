"
I define a parser and code model for MicroDown.
Microdown is a smaller mardown than but mardown, but it is more extensible. 
It supports the default markdown features:
- Header
- Lists
- Paragraph
- Code with arguments: key=val&key2=val2
- Quote
- Table
- Emphasis 
- Figures and links

But also 
- Environment with arguments
- Anchors
- Annotated paragraph
- Math with arguments
- and more intra block elements such as extensible markups, raw, math, and references.

### In Pharo Image Syntax Essence in 2 seconds

```language=undefined

	#Header

	```language=Pharo&caption=Beautiful&label=Fig1
	code
	```
	> Boring quote block 
	> Don't use me!

   [Pharo is cool](http://pharo.org)

   - item 1
     1. sub item 1 
     3. sub item 2
   - item 2 

  `in text` but for pharo hyper links  to class, method and package
  `Point`, `Point class`, `Point>>#setX:setY:`, `#’Microdown-Tests’ (for packages)
```



### Implementation notes

I follow the design mentioned in [https://github.github.com/gfm](https://github.github.com/gfm), in particular the parsing strategy in appendix A.

In short, the strategy is that at any point in time, we might have a number of children of the root which are ""open"". The deepest in open in the tree is called ""current"". All the parents of current are open. 

When a new line is read we do the following:

1. Check if the new line can be consumed by current.
   - as part of this a child of current can be made which can consume the new line
2. If current cannot consume the new line, we close current, move current to its parent, and repeat 1.
3. The root node can consume anything, for instance by making new nodes for storing the new line.
4. The root node is not closed until input is exhausted

The other packages in this repository are the extensions made to produce Pillar model. 
Such packages should be moved in the future to other location (probably pillar itself).

### Acknowledgments
I was implemented by S. Ducasse, L. Dargaud and G. Polito. It is based on the work on markdown parsing of K. Osterbye. 
"
Class {
	#name : #ManifestMicrodown,
	#superclass : #PackageManifest,
	#category : #'Microdown-Manifest'
}
