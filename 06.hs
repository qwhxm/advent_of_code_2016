-- * adventofcode.com/2016/day/6
import Data.Function
import Data.List

-- | messages given as puzzle input
messages :: [String]
messages =
    ["lhyrnfkp", "bdxpggwq", "owtzswtq", "obxtfotk", "hvdshmol", "kpjqpftg",
     "odyagvec", "vqlbxrft", "htwenegq", "pcfoxgwm", "bmnqzuiu", "xidteswq",
     "wiizqpqh", "vnlueoku", "hgzntafs", "kfeswcad", "lqskhvrj", "uguljfpd",
     "dfbmduuu", "gajujozo", "dckmgouu", "hebcfxov", "uwamfjjb", "fjpuapzj",
     "ygxbvkfm", "erbfbfld", "rsspowgs", "zfxrshqw", "gafegcak", "lfcluytl",
     "aqmvtdoc", "dnjwbcit", "jztgwbnc", "tcwuxhjh", "fpfhkcqo", "kachowxl",
     "zlwqlobd", "qfbucjco", "hsmibqak", "ogilflqz", "ivgyuvsc", "flzagyss",
     "oiiboban", "awhilrmt", "ayubmyyr", "utxexzay", "dhmokgfu", "rkkbvaev",
     "fvxlzlze", "wwdlexhb", "yedagwrr", "hhlqfqsh", "ltudvrke", "exhlcdsz",
     "yivivatl", "iwtfslic", "jwvfrjrv", "cxxmyjwc", "scmmaybm", "btiofois",
     "bhwtwamw", "rfukzzou", "qeyteivm", "ukahvjgq", "bgupxqrk", "auftcjcm",
     "yjqhlsnq", "fnwgotrw", "sxlumijz", "hcfzkmwn", "kcadgqti", "qfcjrzdj",
     "eqydvbir", "fdobphcp", "xekmnluy", "clithgmp", "ljstbcqd", "vxjldlan",
     "mgsggpdd", "pjnosake", "qqzturnb", "leheznzo", "ybfofjsq", "zoenlucn",
     "gymggaxt", "zckaufca", "pmedrmhx", "msewtqrf", "eiplbtmg", "oewlpxmw",
     "uunrjsdm", "mawiiqul", "ixglpexk", "fcynrfxy", "bzexbifa", "mowgbeem",
     "gwfyujhp", "jwpuyebl", "avtcspem", "urrvrlvr", "qgvgsknr", "opjkhrao",
     "hoxrpptx", "qhounvgg", "csvdrqqs", "csnbvhvz", "gpcilcdi", "noqqbkxy",
     "smgtokde", "rpgdqnyr", "qbrogtby", "sibulura", "ycwjyxep", "luoyiblx",
     "otqmbbxz", "lupxuwfd", "qbinsygw", "ywvjhhes", "xbmxqhyr", "nbkxytyk",
     "orqhztoz", "crxujqbz", "xodnlvhi", "tykaiqin", "xozmhtae", "xuxnomin",
     "eacuruti", "zkkcbhbn", "tctjwsog", "htgyerci", "sgmgiowl", "vkawsetx",
     "voynmlmc", "fnevibkh", "shvbilhc", "xracdkcd", "fcrdzbbl", "sulzfapg",
     "icdtfcqd", "bqjkzydz", "tablhoxv", "xspyzhwx", "zwlvjfnh", "qnfrpjyb",
     "zvtpcwsy", "ucdbrczy", "uncwjsxx", "ybmlntib", "zfhsohus", "hahcyiwz",
     "nkfwapvp", "dlztazjm", "vsbmddos", "dxtycpao", "cycasgeg", "olhbpcvc",
     "ngwsrotj", "zcxqpkmi", "wshterwt", "pbigbqrt", "sejncntd", "lzfgcsns",
     "husizvtu", "rprjazgp", "beuzgqlw", "tljkixdi", "zbyenvlj", "ffgyvrbk",
     "lwjuypvf", "exnykvdw", "mnnvdqay", "uefjovqd", "zkywlypt", "xuwhirkd",
     "hrdnxbrf", "eozjuiwn", "uxrjjtid", "firfnhfa", "zzojxpnq", "nvurxucq",
     "vdcujaxw", "arssafhy", "vvsfhzee", "ciapvfdc", "qdhlukys", "ksovlkej",
     "pxrwiwzx", "zzfiwfvr", "ayoereac", "ulpvcnro", "rnoikeya", "ovhwczvr",
     "ftqdelxp", "tlbnplxk", "lkgdahfx", "mggihwml", "ksjwayke", "ttuuzpek",
     "eluymfyk", "vdxlaxeu", "wuvfemmu", "yenquvzq", "paezsssc", "oqippwok",
     "itczzoph", "bsccydfh", "tushtlcv", "rfdmmnmd", "hshmdvsj", "vvbrlkrw",
     "ugiswkuu", "kjussibm", "xzsgsnib", "upayqkgu", "wlzpjyiz", "laypqoee",
     "uapvdsne", "tthkhvdr", "iwqovpna", "kfdcmiye", "njvzsbvm", "srhatweg",
     "jsnkvxpl", "zswwzeif", "agbamkem", "djbqhvbv", "vlkjggfu", "hswvkmrj",
     "jbysaclf", "dzwvvziv", "rfczoqnx", "nefkqmww", "uxjobclu", "zwjoigln",
     "hbozuyos", "bdozmqbn", "vhqqedwc", "ljgokudj", "zodxojtk", "nsulycdw",
     "cxzptacm", "cfysgnvq", "dyesppsp", "tkzkmlvh", "oqsdxnkf", "ejgaimum",
     "rjmtdinr", "vpzintix", "gqhymakz", "nwvmscpi", "hqunkejv", "eyhyrcpi",
     "iaflkwqe", "tdxrtdhg", "hfknwepa", "gdxemuby", "mkrajouk", "ajtitfdq",
     "dwcwpggi", "ngimhicq", "udxxyuab", "zwtszrur", "plexfxyv", "wazqfmww",
     "vpognhbz", "vtifgeio", "jldhmfav", "uspahejn", "likckhyf", "jbisjfup",
     "oftrdcwr", "jyqefbhm", "kvdohlkp", "zrumcnji", "nmpaanya", "sggpmlpb",
     "cegbqkqg", "arnnqzsh", "saahxqsp", "izdwddgg", "hfohuypm", "yjvknxyf",
     "bvlwdrmj", "trqabrzq", "pociqnxb", "bmjnhhlx", "msinoxjy", "fooxyuex",
     "pbykwjvs", "cjwhfsgq", "djzxxxjh", "bmjsuoxs", "vtzwpjkf", "ogwnrdzx",
     "zcnjymzg", "pzsfqqex", "yqyfsdyw", "eekchxvn", "ctwktbkp", "whqbkrij",
     "tqrtysfa", "ymndxpzg", "nbiyqgzv", "ekstohsw", "rljlxgpb", "lmrqphro",
     "ouxjemvj", "brrhaful", "enzzllkv", "unmhtfbz", "kjraowee", "jvvuoahh",
     "tfjcyxok", "szgefjvx", "rjsmlsly", "aznbtehx", "idgfmhps", "wxhcjufd",
     "rpkutkqw", "yyettepo", "lflhbnqs", "sdobnamv", "lpxuusdt", "nnlyjlxd",
     "puiriksn", "jmaieiag", "sklextax", "nmznoyfh", "ryaeiofl", "snluyzji",
     "ppnyiohv", "qadwkbhu", "ymuksnww", "yzrplymh", "bdlboxvb", "kiefctje",
     "lwvgitsa", "urudiexs", "tkcxoseq", "mqqzubnu", "pvrfzuyi", "yofpwinb",
     "dvehcoei", "xmpjtcgq", "eyhneuyt", "peronjzx", "cveyxrsp", "jeybgxfh",
     "aqabkslx", "mslqlvuw", "tcprsuco", "bhuuwrhc", "dqxqdqrt", "qupeywul",
     "mvhlcjmv", "ghtpieve", "xiifboln", "mcxhnbib", "oixetmpu", "xmpgndni",
     "bjqdnvrt", "lutqzgkt", "glegjyov", "qtqeqsvf", "adbjbzxh", "tevysrxy",
     "wjwvsawn", "iutrrjqw", "nmnpjusp", "iybibkla", "qvnewyit", "nmailspe",
     "wtlydtcs", "ghormkll", "mfvxauyb", "eaqgafon", "raygyorv", "krmjzdqa",
     "waofjzzp", "ufcveijp", "gzqrvyhl", "rirdkmvf", "miubcwxg", "yuvsrfgp",
     "izeizudk", "incwrrjq", "ereewgbm", "bmvormqn", "parghyrt", "vhttgvgd",
     "dhasugne", "mgbzfmpu", "mybpdtrg", "szjzhabk", "rpkzqopr", "jpqmwony",
     "reneqkqz", "xnocadlb", "mmcauasf", "ngdfnobk", "adleanyx", "dcncwdwh",
     "gnpybbkq", "aulbasht", "njjpuptw", "poxmnfuy", "qnzqruza", "vtkgllko",
     "avewkizl", "iocvvprq", "qcqsdczw", "ehnoydws", "wmlnznpe", "arjqeqqe",
     "gnexqzsj", "tijgmcig", "pbfsvwux", "gugnfhbo", "qmyqyvkm", "fgtckaac",
     "zoeazigt", "qxpizwxb", "poovcaos", "aawpkglf", "fhsdishc", "hxyujvgl",
     "hbmpgqdy", "xuvopjga", "mrfqakoj", "gbhoepag", "fxgqxppa", "rlnyrcnq",
     "idgaetyt", "yiknwvkb", "dsveccnf", "grldsgay", "ydlhxzbg", "zxcrqglb",
     "szmfqqog", "siukuawi", "qjpfebjv", "gyixkejo", "pqmsfrpp", "zbmxowwd",
     "okohvtgz", "prltqpac", "rdlzijcf", "kbcxibir", "yqgolykj", "avkdphjz",
     "cuccogmn", "ihokkctr", "neqbogzz", "cpsqlwff", "nxufhdhw", "fepxflir",
     "rafrmamv", "xoqiyxoz", "jtskidty", "djervlek", "jahstrlh", "ityywfmz",
     "ukkwnapa", "mxwktajk", "vnfnneqe", "webgudjj", "cxtwfeul", "tnzwvzvv",
     "oxgxwsau", "ihivmxwd", "xoatylxh", "wdcaatlt", "cqopcsvz", "zwdmmndm",
     "gogrwieu", "mqsdxygv", "sqpcymua", "jmazibct", "yknevica", "ehzldixx",
     "dyjoghsf", "ikakjhau", "cuwcflmn", "dqxjbpws", "hbtgfrfd", "bksloymh",
     "wdhpbuyy", "wlyulitl", "glgpckqa", "wbyolght", "ccajptsy", "cpaitxsf",
     "aczruvmp", "yktaqanj", "sftudeog", "fnpajmtc", "avbddclh", "ktmfntzj",
     "qykluqnl", "whvdmwsi", "nsitvglv", "pypxhjfc", "fnmwqzrq", "etnoezmk",
     "uvsspzfl", "igdiwpjw", "doajkjck", "jwiwvwot", "wkbkruzo", "thfeynyj",
     "kenadzbr", "jthejixe", "gwlzgutb", "lwmjqqah", "xtkxdnoj", "kohmprey",
     "obqzhykf", "bzvkkedg", "mrmdmdou", "kiacwglo", "xnmfwghi", "crmeazdo",
     "hirvjnhc", "bcghamtz", "dgfxbelb", "jrtrcvio", "dyfvttud", "kkvxqzjm",
     "rcydrpri", "llamtmut", "rwdhzdur", "iikojxou", "xvillkbg", "jorzdbqp",
     "tjryewvm", "jlkvxdmm", "mdsaatua", "shrxrfdp", "fpbvqxch", "emujcgto",
     "gzbrndna", "viqcxvhf", "gpvbnigj", "wpztmzdi", "jpatgigc", "ozrmpbyl",
     "apbfznky", "wkzvbnof", "nhbcplqc", "edehgyfa", "xgdotibf", "tytmwmjd",
     "jxkqfkce", "ferkxtgn", "cyppemii", "vzszchcr", "fiyqcfzj", "kqsjswts",
     "xxmfvbnb", "kpditjko", "qsdseuur", "wfergpjs", "vmwkuxhi", "qkogsmfn",
     "batcpncv", "llpvhscr", "izuikdze", "szujrspz", "kgocdjgb", "euqheofn",
     "kyzbxxqo", "oyislbdo"]

-- | solution to part one of the puzzle
solution1 =
    let
        -- lists of letters in each column
        ms' = transpose messages
        -- function that counts letter frequencies
        freq = map (\cs -> (head cs, length cs)) . group . sort
        -- letter frequencies in each column
        fs = map freq ms'
        -- most frequent letters in each column
        mfcs = map (fst . maximumBy (compare `on` snd)) fs
    in
        mfcs

-- | solution to part two of the puzzle
solution2 =
    let
        -- lists of letters in each column
        ms' = transpose messages
        -- function that counts letter frequencies
        freq = map (\cs -> (head cs, length cs)) . group . sort
        -- letter frequencies in each column
        fs = map freq ms'
        -- least frequent letters in each column
        lfcs = map (fst . minimumBy (compare `on` snd)) fs
    in
        lfcs
