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

class Exception
{
	protected $message;
	protected $code;
	protected $file;
	protected $line;

	public __construct(string $message = "", int $code = 0, Exception $previous = NULL) {}
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

// ==  ==
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

// Note: Generator objects cannot be instantiated via the "new" operator.
// http://php.net/manual/en/class.generator.php
class Generator implements Iterator
{
	public current() {}
	public key() {}
	public next() {}
	public rewind() {}
	public send(mixed $value) {}
	public throw(Exception $exception) {}
	public valid() {}
	public __wakeup() {}
}

?>