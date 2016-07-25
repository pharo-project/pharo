I am serving as a system-wide hub for various tools.
Instead of using direct class names for tools, which is not always nice because can introduce inter-package
dependencies, i provide a uniform way for accessing tools, via Smalltalk global:

Smalltalk tools someToolName,

where 'someToolName' is a name of the tool , under which some tool are registered.

Tools are adding themselves to registry by implementing #registerToolsOn: message at class side, for example:

MyClass>>registerToolsOn: registry
   registry register: self as: #myTool

Registers a MyClass as a tool under name #myTool, and can be accessed via:

Smalltalk tools myTool 

and avoid putting 'MyClass' reference into code.

You can add  ' self registerToolsOn: Smalltalk tools ' in class #initialize,
so your new tool(s) can be registered during package loading.

To reset tool registry to defaults , use: 
Smalltalk resetTools.

Also registry storing a map of inspector types. 
This is to replace an old scheme which using #inspectorClass , and introducing dependencies.
Now, an inspecting class dont needs to have any knowledge about its specialized inspector.
Instead, a specialized inspector could tell registry that it is available for inspecting instances of given class (see #registerInspector:for:  senders).
This allows to avoid dependency from instance class to its inspector class or using extension methods.

WARNING: The point of the tools registry is NOT to be a facade full of protocols but to be a registration for tools that get loaded. So, the tools registry should NOT hold specific tool's protocol.  the tool's client should invoke the correct methods of the tool.

----------- 
The category 'menu' is for backward compatibility with ToolSet. We should remove it.

