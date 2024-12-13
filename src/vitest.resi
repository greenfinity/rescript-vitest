type assertion

module type Asserter = {
  type t<'a>
  let affirm: t<'a> => unit
}

module Runner: (A: Asserter) =>
{
  let test: (string, unit => A.t<_>) => unit
  // let testAsync: (string, ~timeout: int=?, (A.t<_> => unit) => unit) => unit
  let testPromise: (string, ~timeout: int=?, unit => promise<A.t<_>>) => unit
  let testAll: (string, list<'a>, 'a => A.t<_>) => unit
  let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<A.t<_>>) => unit

  let describe: (string, unit => unit) => unit

  @module("vitest") external beforeAll: (unit => unit) => unit = "beforeAll"
  // let beforeAllAsync: (~timeout: int=?, (unit => unit) => unit) => unit
  let beforeAllPromise: (~timeout: int=?, unit => promise<'a>) => unit
  @module("vitest") external beforeEach: (unit => unit) => unit = "beforeEach"
  // let beforeEachAsync: (~timeout: int=?, (unit => unit) => unit) => unit
  let beforeEachPromise: (~timeout: int=?, unit => promise<'a>) => unit
  @module("vitest") external afterAll: (unit => unit) => unit = "afterAll"
  // let afterAllAsync: (~timeout: int=?, (unit => unit) => unit) => unit
  let afterAllPromise: (~timeout: int=?, unit => promise<'a>) => unit
  @module("vitest") external afterEach: (unit => unit) => unit = "afterEach"
  // let afterEachAsync: (~timeout: int=?, (unit => unit) => unit) => unit
  let afterEachPromise: (~timeout: int=?, unit => promise<'a>) => unit

  module Only: {
    let test: (string, unit => A.t<_>) => unit
    // let testAsync: (string, ~timeout: int=?, (A.t<_> => unit) => unit) => unit
    let testPromise: (string, ~timeout: int=?, unit => promise<A.t<_>>) => unit
    let testAll: (string, list<'a>, 'a => A.t<_>) => unit
    let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<A.t<_>>) => unit
    let describe: (string, unit => unit) => unit
  }

  module Skip: {
    let test: (string, unit => A.t<_>) => unit
    // let testAsync: (string, ~timeout: int=?, (A.t<_> => unit) => unit) => unit
    let testPromise: (string, ~timeout: int=?, unit => promise<A.t<_>>) => unit
    let testAll: (string, list<'a>, 'a => A.t<_>) => unit
    let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<A.t<_>>) => unit
    let describe: (string, unit => unit) => unit
  }
}

let test: (string, unit => assertion) => unit
// let testAsync: (string, ~timeout: int=?, (assertion => unit) => unit) => unit
let testPromise: (string, ~timeout: int=?, unit => promise<assertion>) => unit
let testAll: (string, list<'a>, 'a => assertion) => unit
let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<assertion>) => unit

let describe: (string, unit => unit) => unit

@module("vitest") external beforeAll: (unit => unit) => unit = "beforeAll"
let beforeAllPromise: (~timeout: int=?, unit => promise<'a>) => unit
@module("vitest") external beforeEach: (unit => unit) => unit = "beforeEach"
let beforeEachPromise: (~timeout: int=?, unit => promise<'a>) => unit
@module("vitest") external afterAll: (unit => unit) => unit = "afterAll"
let afterAllPromise: (~timeout: int=?, unit => promise<'a>) => unit
@module("vitest") external afterEach: (unit => unit) => unit = "afterEach"
let afterEachPromise: (~timeout: int=?, unit => promise<'a>) => unit

module Only: {
  let test: (string, unit => assertion) => unit
  let testPromise: (string, ~timeout: int=?, unit => promise<assertion>) => unit
  let testAll: (string, list<'a>, 'a => assertion) => unit
  let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<assertion>) => unit
  let describe: (string, unit => unit) => unit
}

module Skip: {
  let test: (string, unit => assertion) => unit
  let testPromise: (string, ~timeout: int=?, unit => promise<assertion>) => unit
  let testAll: (string, list<'a>, 'a => assertion) => unit
  let testAllPromise: (string, list<'a>, ~timeout: int=?, 'a => promise<assertion>) => unit
  let describe: (string, unit => unit) => unit
}

module Todo: {
  let test: string => unit
}

let pass: assertion
let fail: string => assertion

module Expect: {
  type plainPartial<'a> = [#Just('a)]
  type invertedPartial<'a> = [#Not('a)]
  type partial<'a> = [
    | plainPartial<'a>
    | invertedPartial<'a>
  ]

  let expect: 'a => plainPartial<'a>
  let expectFn: ('a => 'b, 'a) => plainPartial<unit => 'b> /* EXPERIMENTAL */

  let toBe: ([< partial<'a>], 'a) => assertion
  let toBeCloseTo: ([< partial<float>], float) => assertion
  let toBeSoCloseTo: ([< partial<float>], ~digits: int, float) => assertion
  let toBeGreaterThan: ([< partial<'a>], 'a) => assertion
  let toBeGreaterThanOrEqual: ([< partial<'a>], 'a) => assertion
  let toBeLessThan: ([< partial<'a>], 'a) => assertion
  let toBeLessThanOrEqual: ([< partial<'a>], 'a) => assertion
  let toBeSupersetOf: ([< partial<array<'a>>], array<'a>) => assertion
  let toContain: ([< partial<array<'a>>], 'a) => assertion
  let toContainEqual: ([< partial<array<'a>>], 'a) => assertion
  let toContainString: ([< partial<string>], string) => assertion
  let toEqual: ([< partial<'a>], 'a) => assertion
  let toHaveLength: ([< partial<array<'a>>], int) => assertion
  let toMatch: ([< partial<string>], string) => assertion
  let toMatchInlineSnapshot: (plainPartial<_>, string) => assertion
  let toMatchRe: ([< partial<string>], Js.Re.t) => assertion
  let toMatchSnapshot: plainPartial<_> => assertion
  let toMatchSnapshotWithName: (plainPartial<_>, string) => assertion
  let toThrow: [< partial<unit => _>] => assertion
  let toThrowErrorMatchingSnapshot: plainPartial<unit => _> => assertion

  let not_: plainPartial<'a> => invertedPartial<'a>
  let not__: plainPartial<'a> => invertedPartial<'a>

  module Operators: {
    @@ocaml.text(" experimental ")

    let \"==": ([< partial<'a>], 'a) => assertion
    let \">": ([< partial<'a>], 'a) => assertion
    let \">=": ([< partial<'a>], 'a) => assertion
    let \"<": ([< partial<'a>], 'a) => assertion
    let \"<=": ([< partial<'a>], 'a) => assertion
    let \"=": ([< partial<'a>], 'a) => assertion
    let \"<>": (plainPartial<'a>, 'a) => assertion
    let \"!=": (plainPartial<'a>, 'a) => assertion
  }
}

module ExpectJs: {
  include module type of Expect

  let toBeDefined: [< partial<Js.undefined<_>>] => assertion
  let toBeFalsy: [< partial<_>] => assertion
  let toBeNull: [< partial<Js.null<_>>] => assertion
  let toBeTruthy: [< partial<_>] => assertion
  let toBeUndefined: [< partial<Js.undefined<_>>] => assertion
  let toContainProperties: ([< partial<{..}>], array<string>) => assertion
  let toMatchObject: ([< partial<{..}>], {..}) => assertion
}

module MockJs: {
  @@ocaml.text(" experimental ")

  type fn<'fn, 'args, 'ret>

  let new0: fn<unit => 'ret, unit, 'ret> => 'ret
  let new1: (fn<'a => 'ret, 'a, 'ret>, 'a) => 'ret
  let new2: (fn<('a, 'b) => 'ret, ('a, 'b), 'ret>, 'a, 'b) => 'ret

  external fn: fn<'fn, _, _> => 'fn = "%identity"
  let calls: fn<_, 'args, _> => array<'args>
  let instances: fn<_, _, 'ret> => array<'ret>

  @ocaml.doc(" Beware: this actually replaces `mock`, not just `mock.instances` and `mock.calls` ")
  @send
  external mockClear: fn<'fn, 'a, 'b> => unit = "mockClear"
  @send external mockReset: fn<'fn, 'a, 'b> => unit = "mockReset"
  @send external mockImplementation: (fn<'fn, 'a, 'b> as 'self, 'fn) => 'self = "mockImplementation"
  @send
  external mockImplementationOnce: (fn<'fn, _, _> as 'self, 'fn) => 'self = "mockImplementationOnce"
  @send
  external mockReturnThis: fn<_, _, 'ret> => 'ret =
    "mockReturnThis" /* not type safe, we don't know what `this` actually is */
  @send external mockReturnValue: (fn<_, _, 'ret> as 'self, 'ret) => 'self = "mockReturnValue"
  @send
  external mockReturnValueOnce: (fn<_, _, 'ret> as 'self, 'ret) => 'self = "mockReturnValueOnce"
}

module Vi: {
  type fakeTimerImplementation = [#legacy | #modern]
  @module("vitest") @scope("vi") external clearAllTimers: unit => unit = "clearAllTimers"
  @module("vitest") @scope("vi") external runAllTicks: unit => unit = "runAllTicks"
  @module("vitest") @scope("vi") external runAllTimers: unit => unit = "runAllTimers"
  // @module("vitest") @scope("vi") external runAllImmediates: unit => unit = "runAllImmediates"
  @module("vitest") @scope("vi") external runTimersToTime: int => unit = "runTimersToTime"
  @module("vitest") @scope("vi") external advanceTimersByTime: int => unit = "advanceTimersByTime"
  @module("vitest") @scope("vi")
  external runOnlyPendingTimers: unit => unit = "runOnlyPendingTimers"
  @module("vitest") @scope("vi")
  external useFakeTimersImplementation: fakeTimerImplementation => unit = "useFakeTimers"
  let useFakeTimers: (~implementation: fakeTimerImplementation=?, unit) => unit
  @module("vitest") @scope("vi") external useRealTimers: unit => unit = "useRealTimers"
  type systemTime = [#int(int) | #date(Js.Date.t)]
  let setSystemTime: systemTime => unit
}

module Mock: {
  @@ocaml.text(" experimental ")

  @module("vitest") external disableAutomock: unit => unit = "disableAutomock"
  @module("vitest") external enableAutomock: unit => unit = "enableAutomock"
  @module("vitest") external resetModules: unit => unit = "resetModules"
  @module("vitest") @scope("vi")
  external inferred_fn: unit => MockJs.fn<'a => Js.undefined<'b>, 'a, Js.undefined<'b>> = "fn"
  @module("vitest") @scope("vi") external fn: ('a => 'b) => MockJs.fn<'a => 'b, 'a, 'b> = "fn"
  @module("vitest") @scope("vi")
  external fn2: (('a, 'b) => 'c) => MockJs.fn<('a, 'b) => 'c, ('a, 'b), 'c> = "fn"
  @module("vitest") external mock: string => unit = "mock"
  @module("vitest") external mockWithFactory: (string, unit => 'a) => unit = "mock"
  @module("vitest") external mockVirtual: (string, unit => 'a, {..}) => unit = "mock"
  @module("vitest") external clearAllMocks: unit => unit = "clearAllMocks"
  @module("vitest") external resetAllMocks: unit => unit = "resetAllMocks"
  @module("vitest") external setMock: (string, {..}) => unit = "setMock"
  @module("vitest") external unmock: string => unit = "unmock"
  @module("vitest")
  external spyOn: ({..} as 'this, string) => MockJs.fn<unit, unit, 'this> = "spyOn"
}
