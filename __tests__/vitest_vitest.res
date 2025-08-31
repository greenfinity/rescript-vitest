open Vitest
open Expect
// open! Expect.Operators
module Promise = Js.Promise

@val external setTimeout: (unit => unit, int) => unit = "setTimeout"
@val external setImmediate: (unit => unit) => unit = "setImmediate"
@val external nextTick: (unit => unit) => unit = "process.nextTick"

let () = describe("Fake Timers", () => {
  test("runAllTimers", () => {
    let flag = ref(false)
    Vi.useFakeTimers()
    setTimeout(() => flag := true, 0)
    let before = flag.contents
    Vi.runAllTimers()

    expect((before, flag.contents))->toEqual((false, true))
  })

  testPromise("runAllTicks", async () => {
    let flag = ref(false)
    Vi.useFakeTimers()
    nextTick(() => flag := true)
    let before = flag.contents
    Vi.runAllTicks()

    await Promise.make(
      (~resolve, ~reject as _) =>
        nextTick(() => resolve(expect((before, flag.contents))->toEqual((false, true)))),
    )
  })

  // test("runAllImmediates ", () => {
  //   let flag = ref(false)
  //   Vi.useFakeTimers(~implementation=#legacy, ())
  //   setImmediate(() => flag := true)
  //   let before = flag.contents
  //   Vi.runAllImmediates()

  //   expect((before, flag.contents)) == (false, true)
  // })

  test("runTimersToTime", () => {
    let flag = ref(false)
    Vi.useFakeTimers(~implementation=#legacy, ())
    setTimeout(() => flag := true, 1500)
    let before = flag.contents
    Vi.advanceTimersByTime(1000)
    let inbetween = flag.contents
    Vi.advanceTimersByTime(1000)

    expect((before, inbetween, flag.contents))->toEqual((false, false, true))
  })

  test("advanceTimersByTime", () => {
    let flag = ref(false)
    Vi.useFakeTimers(~implementation=#legacy, ())
    setTimeout(() => flag := true, 1500)
    let before = flag.contents
    Vi.advanceTimersByTime(1000)
    let inbetween = flag.contents
    Vi.advanceTimersByTime(1000)

    expect((before, inbetween, flag.contents))->toEqual((false, false, true))
  })

  test("runOnlyPendingTimers", () => {
    let count = ref(0)
    Vi.useFakeTimers(~implementation=#legacy, ())
    let rec recursiveTimeout = () => {
      count := count.contents + 1
      setTimeout(recursiveTimeout, 1500)
    }
    recursiveTimeout()
    let before = count.contents
    Vi.runOnlyPendingTimers()
    let inBetween = count.contents
    Vi.runOnlyPendingTimers()

    expect((before, inBetween, count.contents))->toEqual((1, 2, 3))
  })

  test("clearAllTimers", () => {
    let flag = ref(false)
    Vi.useFakeTimers()
    setImmediate(() => flag := true)
    let before = flag.contents
    Vi.clearAllTimers()
    Vi.runAllTimers()

    expect((before, flag.contents))->toEqual((false, false))
  })

  testPromise("clearAllTimers", async () => {
    Vi.useFakeTimers(~implementation=#legacy, ())
    Vi.useRealTimers()

    await Promise.make((~resolve, ~reject as _) => nextTick(() => resolve(pass)))
  })
})

describe("affirm", () => {
  test("affirm pass", () => {
    expect(1)->toEqual(1)->affirm
    pass
  })

  test("affirm fail", () =>
    try {
      expect(1)->toEqual(2)->affirm
      fail("should have thrown")
    } catch {
    | _ => pass
    }
  )
})
