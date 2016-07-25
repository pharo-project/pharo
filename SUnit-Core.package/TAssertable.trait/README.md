I think it is not used yet. I think it is a Trait so that we can make things assertable and not need to subclass from TestCase for example. I may expect that TestCase uses such Trait instead of implementing all those methods,  but it is not the case. Even more, TAssertable users size -> 0. 

So...seems a work half done. Adrian Kuhn introduce it. Check
http://forum.world.st/template/NamlServlet.jtp?macro=search_page&node=1294837&query=TAssertable