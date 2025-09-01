open Vitest
module Promise = Js.Promise

let () = {
  test("pass", () => pass)

  Skip.test("fail", () => fail(""))

  test("test", () => pass)

  Skip.test("test - expect fail", () => fail(""))

  testPromise("testPromise", () => Promise.resolve(pass))

  Skip.testPromise("testPromise - reject", () => Promise.reject(Failure("")))

  Skip.testPromise("testPromise - expect fail", () => Promise.resolve(fail("")))

  testPromise("testPromise - timeout ok", ~timeout=1, () => Promise.resolve(pass))

  Skip.testPromise("testPromise - timeout fail", ~timeout=1, () =>
    Promise.make((~resolve as _, ~reject as _) => ())
  )

  testAll("testAll", list{"foo", "bar", "baz"}, input =>
    if Js.String.length(input) === 3 {
      pass
    } else {
      fail("")
    }
  )
  testAll("testAll - tuples", list{("foo", 3), ("barbaz", 6), ("bananas!", 8)}, ((input, output)) =>
    if Js.String.length(input) === output {
      pass
    } else {
      fail("")
    }
  )

  testAllPromise("testAllPromise", list{"foo", "bar", "baz"}, input =>
    Promise.resolve(
      if Js.String.length(input) === 3 {
        pass
      } else {
        fail("")
      },
    )
  )
  testAllPromise("testAllPromise - tuples", list{("foo", 3), ("barbaz", 6), ("bananas!", 8)}, ((
    input,
    output,
  )) =>
    Promise.resolve(
      if Js.String.length(input) === output {
        pass
      } else {
        fail("")
      },
    )
  )

  describe("describe", () => test("some aspect", () => pass))

  describePromise("describePromise", async () => test("some aspect", () => pass))

  describe("beforeAll", () => {
    let x = ref(0)

    beforeAll(() => x := x.contents + 4)

    test("x is 4", () =>
      if x.contents === 4 {
        pass
      } else {
        fail("")
      }
    )
    test("x is still 4", () =>
      if x.contents === 4 {
        pass
      } else {
        fail("")
      }
    )
  })

  describe("beforeAllPromise", () => {
    describe("without timeout", () => {
      let x = ref(0)

      beforeAllPromise(
        () => {
          x := x.contents + 4
          Promise.resolve()
        },
      )

      test(
        "x is 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is still 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
    })

    describe("with 100ms timeout", () => {
      let x = ref(0)

      beforeAllPromise(
        ~timeout=100,
        () => {
          x := x.contents + 4
          Promise.resolve()
        },
      )

      test(
        "x is 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is still 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
    })

    Skip.describe("timeout should fail suite", () => {
      beforeAllPromise(~timeout=1, () => Promise.make((~resolve as _, ~reject as _) => ()))
      test("", () => pass) /* runner will crash if there's no tests */
    })
  })

  describe("beforeEach", () => {
    let x = ref(0)

    beforeEach(() => x := x.contents + 4)

    test("x is 4", () =>
      if x.contents === 4 {
        pass
      } else {
        fail("")
      }
    )
    test("x is suddenly 8", () =>
      if x.contents === 8 {
        pass
      } else {
        fail("")
      }
    )
  })

  describe("beforeEachPromise", () => {
    describe("without timeout", () => {
      let x = ref(0)

      beforeEachPromise(
        () => {
          x := x.contents + 4
          Promise.resolve(true)
        },
      )

      test(
        "x is 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is suddenly 8",
        () =>
          if x.contents === 8 {
            pass
          } else {
            fail("")
          },
      )
    })

    describe("with 100ms timeout", () => {
      let x = ref(0)

      beforeEachPromise(
        ~timeout=100,
        () => {
          x := x.contents + 4
          Promise.resolve(true)
        },
      )

      test(
        "x is 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is suddenly 8",
        () =>
          if x.contents === 8 {
            pass
          } else {
            fail("")
          },
      )
    })

    Skip.describe("timeout should fail suite", () => {
      beforeEachPromise(~timeout=1, () => Promise.make((~resolve as _, ~reject as _) => ()))
      test("", () => pass) /* runner will crash if there's no tests */
    })
  })

  describe("afterAll", () => {
    let x = ref(0)

    describe("phase 1", () => {
      afterAll(() => x := x.contents + 4)

      test(
        "x is 0",
        () =>
          if x.contents === 0 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is still 0",
        () =>
          if x.contents === 0 {
            pass
          } else {
            fail("")
          },
      )
    })

    describe("phase 2", () =>
      test(
        "x is suddenly 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
    )
  })

  describe("afterAllPromise", () => {
    describe("without timeout", () => {
      let x = ref(0)

      describe(
        "phase 1",
        () => {
          afterAllPromise(
            () => {
              x := x.contents + 4
              Promise.resolve(true)
            },
          )

          test(
            "x is 0",
            () =>
              if x.contents === 0 {
                pass
              } else {
                fail("")
              },
          )
          test(
            "x is still 0",
            () =>
              if x.contents === 0 {
                pass
              } else {
                fail("")
              },
          )
        },
      )

      describe(
        "phase 2",
        () =>
          test(
            "x is suddenly 4",
            () =>
              if x.contents === 4 {
                pass
              } else {
                fail("")
              },
          ),
      )
    })

    describe("with 100ms timeout", () => {
      let x = ref(0)

      describe(
        "phase 1",
        () => {
          afterAllPromise(
            ~timeout=100,
            () => {
              x := x.contents + 4
              Promise.resolve(true)
            },
          )

          test(
            "x is 0",
            () =>
              if x.contents === 0 {
                pass
              } else {
                fail("")
              },
          )
          test(
            "x is still 0",
            () =>
              if x.contents === 0 {
                pass
              } else {
                fail("")
              },
          )
        },
      )

      describe(
        "phase 2",
        () =>
          test(
            "x is suddenly 4",
            () =>
              if x.contents === 4 {
                pass
              } else {
                fail("")
              },
          ),
      )
    })

    Skip.describe("timeout should fail suite", () => {
      afterAllPromise(~timeout=1, () => Promise.make((~resolve as _, ~reject as _) => ()))
      test("", () => pass) /* runner will crash if there's no tests */
    })
  })

  describe("afterEach", () => {
    let x = ref(0)

    afterEach(() => x := x.contents + 4)

    test("x is 0", () =>
      if x.contents === 0 {
        pass
      } else {
        fail("")
      }
    )
    test("x is suddenly 4", () =>
      if x.contents === 4 {
        pass
      } else {
        fail("")
      }
    )
  })

  describe("afterEachPromise", () => {
    describe("without timeout", () => {
      let x = ref(0)

      afterEachPromise(
        () => {
          x := x.contents + 4
          Promise.resolve(true)
        },
      )

      test(
        "x is 0",
        () =>
          if x.contents === 0 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is suddenly 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
    })

    describe("with 100ms timeout", () => {
      let x = ref(0)

      afterEachPromise(
        ~timeout=100,
        () => {
          x := x.contents + 4
          Promise.resolve(true)
        },
      )

      test(
        "x is 0",
        () =>
          if x.contents === 0 {
            pass
          } else {
            fail("")
          },
      )
      test(
        "x is suddenly 4",
        () =>
          if x.contents === 4 {
            pass
          } else {
            fail("")
          },
      )
    })

    Skip.describe("timeout should fail suite", () => {
      afterEachPromise(~timeout=1, () => Promise.make((~resolve as _, ~reject as _) => ()))
      test("", () => pass) /* runner will crash if there's no tests */
    })
  })

  describe("Only", () =>
    /* See globals_only_test.ml */
    test("", () => pass)
  )

  describe("Skip", () => {
    Skip.test("Skip.test", () => pass)

    Skip.testPromise("Skip.testPromise", () => Promise.resolve(pass))
    Skip.testPromise("testPromise - timeout", ~timeout=1, () =>
      Promise.make((~resolve as _, ~reject as _) => ())
    )

    Skip.testAll("testAll", list{"foo", "bar", "baz"}, input =>
      if Js.String.length(input) === 3 {
        pass
      } else {
        fail("")
      }
    )
    Skip.testAll("testAll - tuples", list{("foo", 3), ("barbaz", 6), ("bananas!", 8)}, ((
      input,
      output,
    )) =>
      if Js.String.length(input) === output {
        pass
      } else {
        fail("")
      }
    )
    Skip.testAllPromise("testAllPromise", list{"foo", "bar", "baz"}, input =>
      Promise.resolve(
        if Js.String.length(input) === 3 {
          pass
        } else {
          fail("")
        },
      )
    )
    Skip.testAllPromise(
      "testAllPromise - tuples",
      list{("foo", 3), ("barbaz", 6), ("bananas!", 8)},
      ((input, output)) =>
        Promise.resolve(
          if Js.String.length(input) === output {
            pass
          } else {
            fail("")
          },
        ),
    )

    Skip.describe("Skip.describe", () => test("some aspect", () => pass))
  })

  describe("Todo", () => Todo.test("Todo.test"))
}
