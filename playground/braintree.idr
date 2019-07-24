module braintree

data Objective = GeekAlpha | ConcurrentDeveloper | LinuxMaster | AiEngiennier | ProductOwner | FpDynamic | FpTypes

data Math = Proof
data Py = IntensivePython
data Ai = DeepLearing | PyTorch | DeepReinforcementLearning

total math_for_programmers : Math -> (Math, Py)
math_for_programmers Proof = (Proof, IntensivePython)

total pythonic_line : (Math, Py) -> (Math, Py, List Ai)
pythonic_line (Proof, IntensivePython) =
  (Proof, IntensivePython, [DeepLearing, DeepReinforcementLearning, PyTorch])

total ai_engiennier : (Math, Py, List Ai) -> Objective
ai_engiennier learn = AiEngiennier

-- ai_engiennier pythonic_line math_for_programmers Proof

data Reative = ReativePatterns
data Actors = AkkaInAction
total reactive_line : Reative -> Actors
reactive_line ReativePatterns = AkkaInAction

data Product = TheHand
data Web = EssentialPlay
data Model = DDDFuncional
web_server_line : Web -> Model -> Product
web_server_line EssentialPlay DDDFuncional = TheHand

data Clojure = BraveClojure
total dynamic_funtional : Clojure -> Objective
dynamic_funtional BraveClojure = FpDynamic

data Scala = FunctionalScala
data Types = Idris
data PureScala = ScalazForHumans | EssentialCats
data Pure = HaskellManning | HaskellFirstPrinciples

total functional_base : Scala -> Types -> Pure
functional_base FunctionalScala Idris = HaskellManning
functional_base FunctionalScala Idris = HaskellFirstPrinciples

functional_line : (PureScala, Pure) -> Objective
functional_line (ScalazForHumans, p) = FpTypes
functional_line (EssentialCats, p) = FpTypes

data Cpp = ApiLinux | FuncionalCpp | ConcurrencyCpp | CUDA
data OpenSource = ForgeOpenSource

total omega_reason : Cpp -> Objective
omega_reason ApiLinux = LinuxMaster
omega_reason FuncionalCpp = FpTypes
omega_reason ConcurrencyCpp = ConcurrentDeveloper
omega_reason CUDA = ConcurrentDeveloper

total geek_aplha_reason : OpenSource -> Objective
geek_aplha_reason ForgeOpenSource = GeekAlpha
