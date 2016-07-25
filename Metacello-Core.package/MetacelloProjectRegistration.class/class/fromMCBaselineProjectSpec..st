instance creation
fromMCBaselineProjectSpec: aProjectSpec
    ^ self new
        projectName: aProjectSpec name;
        baselineProjectSpec: aProjectSpec;
        yourself