private
privateStyle: aText 
    | ast |

    ast := self parse: aText isMethod: workspace isNil.
    ast methodNode compilationContext: (CompilationContext new
            class: (classOrMetaClass ifNil: [UndefinedObject]);
				forSyntaxHighlighting: true;
            requestor: workspace).
    ast doSemanticAnalysis.
    self style: aText ast: ast.
    ^ aText