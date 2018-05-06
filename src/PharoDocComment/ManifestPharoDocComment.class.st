"
This package supports the handling (syntax highlighting and first class manipulation) of small executable examples inline in method comment. 

Assuming the following method is defined

AbstractFileReference >> basename
            ""Returns the basename, i.e. /foo/gloops.taz basename is 'gloops.taz'""
            ""'/foo/gloops.taz' asFileReference basename >>> 'gloops.taz'""
            ^ self fullPath basename

The following expression allows one to access to the expression and its expected result. 

[[[
| doc |
doc := (AbstractFileReference >> #basename) ast pharoDocCommentNodes.
doc first expression 
> '/foo/gloops.taz' asFileReference basename
doc result
>  'gloops.taz'
]]]

The code browser will show a little icon with the executed examples.  In that case you will get the original expression, its returned result and the expected result. 

Now you can also execute manually the expression and you will get a pair containing the result of the 


Implementation Notes

- may be PharoDocCommentNode and PharoDocCommentExpression should be subclass of ProgramNode.
- questions for nicolai
	it would be nice to address 
		example expression expressionCode 
- I renamed expression and result into expressionNode, resultNode. To be able to make the difference between an expression, its expected value and its effective value. 		
	
"
Class {
	#name : #ManifestPharoDocComment,
	#superclass : #PackageManifest,
	#category : #'PharoDocComment-Manifest'
}
