scripting
copyForScriptingInto: aProjectSpec
    ^aProjectSpec
        setName: name;
        className: className;
        versionString: versionString;
        operator: operator;
        setLoads: loads;
        preLoadDoIt: preLoadDoIt;
        postLoadDoIt: postLoadDoIt;
        repositories: repositories copy;
        file: file