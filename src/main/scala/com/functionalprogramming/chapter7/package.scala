package com.functionalprogramming

import java.util.concurrent.{Future, ExecutorService}

package object chapter7 {
  type Par[A] = ExecutorService => Future[A]
}
