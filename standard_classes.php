<?php

// Declaration file for standard PHP classes and classes of common extensions

// == "Core" ==
class stdClass
{
}

class __PHP_Incomplete_Class
{
}

class Directory
{
	public $path;
	public $handle;

	public close(resource $dir_handle = $this) {}
	public read(resource $dir_handle = $this) {}
	public rewind(resouce $dir_handle = $this) {}
}

class Throwable
{
	abstract public getMessage() {}
	abstract public getCode() {}
	abstract public getFile() {}
	abstract public getLine() {}
	abstract public getTrace() {}
	abstract public getTraceAsString() {}
	abstract public getPrevious() {}
	abstract public __toString() {}
}

class Exception extends Throwable
{
	protected $message;
	protected $code;
	protected $file;
	protected $line;

	public __construct(string $message = "", int $code = 0, Throwable $previous = NULL) {}
	final public getMessage() {}
	final public getPrevious() {}
	final public getCode() {}
	final public getFile() {}
	final public getLine() {}
	final public getTrace() {}
	final public getTraceAsString() {}
	public __toString() {}
	final private __clone() {}
}

class Error extends Throwable
{
	protected $message;
	protected $code;
	protected $file;
	protected $line;
	public __construct(string $message = "" , int $code = 0 , Throwable $previous = NULL)
	final public getMessage() {}
	final public getPrevious() {}
	final public getCode() {}
	final public getFile() {}
	final public getLine() {}
	final public getTrace() {}
	final public getTraceAsString() {}
	public __toString() {}
	final private __clone() {}
}

class ErrorException extends Exception
{
	protected $severity;

	public __construct(string $message = "", int $code = 0, int $severity = 1, string $filename = __FILE__, int $lineno = __LINE__, Exception $previous = NULL) {}
	final public getSeverity() {}
}

class php_user_filter
{
	public $filtername;
	public $params;

	public filter(resource $in, resource $out, int &$consumed, bool $closing) {}
	public onClose() {}
	public onCreate() {}
}

class Closure
{
	private __construct() {}
	public static bind(Closure $closure, object $newthis, mixed $newscope = "static") {}
	public bindTo(object $newthis, mixed $newscope = "static") {}
}

// == SPL Exceptions ==
class BadFunctionCallException extends LogicException
{
}

class BadMethodCallException extends BadFunctionCallException
{
}

class DomainException extends LogicException
{
}

class InvalidArgumentException extends LogicException
{
}

class LengthException extends LogicException
{
}

class LogicException extends Exception
{
}

class OutOfBoundsException extends RuntimeException
{
}

class OutOfRangeException extends LogicException
{
}

class OverflowException extends RuntimeException
{
}

class RangeException extends RuntimeException
{
}

class RuntimeException extends Exception
{
}

class UnderflowException extends RuntimeException
{
}

class UnexpectedValueException extends RuntimeException
{
}

// == Errors ==
class TypeError extends Error
{
}

class ParseError extends Error
{
}

class AssertionError extends Error
{
}

class ArithmeticError extends Error
{
}

class DivisionByZeroError extends Error
{
}

class CompileError extends Error
{
}

// == Iterators ==
class Traversable
{
}

class Iterator extends Traversable
{
	abstract public current() {}
	abstract public key() {}
	abstract public next() {}
	abstract public rewind() {}
	abstract public valid() {}
}


class IteratorAggregate extends Traversable
{
	abstract public getIterator() {}
}

class ArrayIterator extends IteratorAggregate
{
	const STD_PROP_LIST = 1;
	const ARRAY_AS_PROPS = 2;

	public append(mixed $value) {}
	public asort() {}
	public __construct(mixed $array = array(), int $flags = 0) {}
	public count() {}
	public current() {}
	public getArrayCopy() {}
	public getFlags() {}
	public key() {}
	public ksort() {}
	public natcasesort() {}
	public natsort() {}
	public next() {}
	public offsetExists(mixed $index) {}
	public offsetGet(mixed $index) {}
	public offsetSet(mixed $index, mixed $newval) {}
	public offsetUnset(mixed $index) {}
	public rewind() {}
	public seek(int $position) {}
	public serialize() {}
	public setFlags(string $flags) {}
	public uasort(callable $cmp_function) {}
	public uksort(callable $cmp_function) {}
	public unserialize(string $serialized) {}
	public valid() {}
}

// Note: Generator objects cannot be instantiated via the "new" operator.
// http://php.net/manual/en/class.generator.php
class Generator extends Iterator
{
	public current() {}
	public getReturn() {} // PHP7
	public key() {}
	public next() {}
	public rewind() {}
	public send(mixed $value) {}
	public throw(Throwable $exception) {}
	public valid() {}
	public __wakeup() {}
}

// == SPL classes ==
class ArrayObject
{
	const STD_PROP_LIST = 1;
	const ARRAY_AS_PROPS = 2;
	
	public __construct(mixed $input = array(), int $flags = 0, string $iterator_class = "ArrayIterator") {}
	public append(mixed $value) {}
	public asort() {}
	public count() {}
	public exchangeArray(mixed $input) {}
	public getArrayCopy() {}
	public getFlags() {}
	public getIterator() {}
	public getIteratorClass() {}
	public ksort() {}
	public natcasesort() {}
	public natsort() {}
	public offsetExists(mixed $index) {}
	public offsetGet(mixed $index) {}
	public offsetSet(mixed $index, mixed $newval) {}
	public offsetUnset(mixed $index) {}
	public sserialize() {}
	public setFlags(int $flags) {}
	public setIteratorClass(string $iterator_class) {}
	public uasort(callable $cmp_function) {}
	public uksort(callable $cmp_function) {}
	public unserialize(string $serialized) {}
}

?>
