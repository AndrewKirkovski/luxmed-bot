package com.lbs.server.service

import org.springframework.stereotype.Service

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

/** Serializes a legacy monitor, deactivation and REST booking for one account. */
@Service
class AccountBookingFence {
  private val locks = new ConcurrentHashMap[java.lang.Long, ReentrantLock]()

  def withLock[A](accountId: Long)(body: => A): A = {
    val key = Long.box(accountId)
    val created = new ReentrantLock()
    val existing = locks.putIfAbsent(key, created)
    val lock = if (existing == null) created else existing
    lock.lock()
    try body finally lock.unlock()
  }
}
