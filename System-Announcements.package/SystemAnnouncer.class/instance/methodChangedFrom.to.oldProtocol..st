triggering
methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol

	self announce: (MethodModified methodChangedFrom: oldMethod to: newMethod oldProtocol: oldProtocol)