return {
    Game = {
        binding = "Game",
        metadata = {
            ["fls/itemKind"] = "Variable",
            ["fnl/docstring"] = "The active Kristal game object."
        }
    },
    Registry = {
        binding = "Registry",
        metadata = {
            ["fls/itemKind"] = "Module",
            ["fnl/docstring"] = "Kristal's runtime content registry."
        }
    },
    Mod = {
        binding = "Mod",
        metadata = {
            ["fls/itemKind"] = "Module",
            ["fnl/docstring"] = "The currently loaded Kristal mod."
        }
    },
    Kristal = {
        binding = "Kristal",
        metadata = {
            ["fls/itemKind"] = "Module",
            ["fnl/docstring"] = "Kristal engine entry points."
        },
        fields = {
            quickReload = {
                binding = "Kristal.quickReload",
                metadata = {
                    ["fls/itemKind"] = "Function",
                    ["fnl/arglist"] = {"mode"},
                    ["fnl/docstring"] = "Reload the current mod using temp, save, or none."
                }
            }
        }
    }
}
