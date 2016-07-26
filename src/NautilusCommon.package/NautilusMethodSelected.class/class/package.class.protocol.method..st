instance creation
package: aPackage class: aClass protocol: aProtocol method: aMethod

	^ self new
		selectedPackage: aPackage;
		selectedClass: aClass;
		selectedProtocol: aProtocol;
		selectedMethod: aMethod;
		yourself