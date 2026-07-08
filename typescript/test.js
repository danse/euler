import { test } from 'node:test'
import { strict as assert } from 'node:assert'
const i = await import('./index.js')

test('rightTriangleSolutions', (t) => {
  assert.equal(i.rightTriangleSolutions(120), 3)
})

test('circularPrimes', (t) => {
  assert.equal(i.circularPrimes(100), 13)
})

test('splitAt', (t) => {
  assert.deepEqual(i.splitAt('abc', 1), ['a', 'bc'])
})

test('perm', (t) => {
  const e = [
    'abc',
    'acb',
    'bac',
    'bca',
    'cab',
    'cba'
  ]
  assert.deepEqual(i.perm(['a', 'b', 'c']), e)
})

test('coinSums', (t) => {
  assert.equal(i.coinSums(0), 1)
  assert.equal(i.coinSums(2), 2)
  assert.equal(i.coinSums(3), 2)
})

test('binaryMask', (t) => {
  const e2 = [
    [false, false],
    [false, true],
    [true, false],
    [true, true]
  ]
  assert.deepEqual(i.binaryMask(2), e2)
  const e3 = [
    [false, false, false],
    [false, false, true],
    [false, true, false],
    [false, true, true],
    [true, false, false],
    [true, false, true],
    [true, true, false],
    [true, true, true]
  ]
  assert.deepEqual(i.binaryMask(3), e3)
})

test('distinctPowers', (t) => {
  assert.equal(i.distinctPowers(5), 15)
})

test('spiralDiagonals', (t) => {
  assert.equal(i.spiralDiagonals(5), 101)
})

test('consecutivePrimes', (t) => {
  assert.equal(i.consecutivePrimes(-79, 1601), 80)
})

test('reciprocalCycles', (t) => {
  assert.equal(i.reciprocalCycles(10), 7)
})

test('periodicDiv + extractCycle', (t) => {
  assert.deepEqual(
    i.extractCycle(i.periodicDiv(1, 7)),
    [1,4,2,8,5,7])
})

test('periodicDiv', (t) => {
  assert.deepEqual(
    i.periodicDiv(1, 7),
    [1,4,2,8,5,7,1,4,2,8,5,7])
})

test('digitFibonacci', (t) => {
  assert.equal(i.digitFibonacci(3), 12)
})

test('lexicographicPermutations', (t) => {
  assert.equal(i.lexicographicPermutations(["0", "1", "2"], 5), '201')
})

test('alphabeticalValue', (t) => {
  assert.equal(i.alphabeticalValue('COLIN'), 53)
})

test('factorialDigitsSum', (t) => {
  assert.equal(i.factorialDigitsSum(10), 27)
})

test('numberLetterCounts', (t) => {
  assert.equal(i.numberLetterCounts(5), 19)
})

test('latticePaths', (t) => {
  assert.equal(i.latticePaths(2), 6)
})

test('collatzLength', (t) => {
  assert.equal(i.collatzLength(13), 10)
})

test ('divisors', (t) => {
  assert.equal(i.divisors(28).length, 6)
})

test('summationOfPrimes', (t) => {
  assert.equal(i.summationOfPrimes(10), 17)
})

test('nthPrime', (t) => {
  assert.equal(i.nthPrime(6), 13)
})

test('isPrime', (t) => {
  assert.equal(i.isPrime(0), false)
  assert.equal(i.isPrime(2), true)
  assert.equal(i.isPrime(4), false)
  assert.equal(i.isPrime(13), true)
})

test('sumSquareDifference', (t) => {
  assert.equal(i.sumSquareDifference(10), 2640)
})

test ('smallestMultiple', (t) => {
  assert.equal(i.smallestMultiple(10), 2520)
})

test('isPalindrome', (t) => {
  assert.equal(i.isPalindrome('64146'), true)
})

test('largestPrimeFactor', (t) => {
  assert.equal(i.largestPrimeFactor(13195), 29)
})

test('f1', (t) => {
  assert.equal(i.f1(10), 23)
})
